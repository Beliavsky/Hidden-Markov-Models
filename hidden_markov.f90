module hidden_markov_mod
  use kind_mod, only: dp
  use random_mod, only: random_normal
  implicit none
  private
  public :: simulate_hmm, stationary

contains

  subroutine simulate_hmm(n, init_prob, trans_mat, means, sds, nobs, states, obs)
    ! Simulate a hidden Markov model with Gaussian observations
    integer, intent(in) :: n                   ! number of states
    real(kind=dp), intent(in) :: init_prob(n) ! initial state probabilities
    real(kind=dp), intent(in) :: trans_mat(n,n) ! transition probability matrix
    real(kind=dp), intent(in) :: means(n)     ! state means
    real(kind=dp), intent(in) :: sds(n)       ! state standard deviations
    integer, intent(in) :: nobs                ! number of observations
    integer, intent(out) :: states(nobs)       ! simulated states
    real(kind=dp), intent(out) :: obs(nobs)    ! simulated observations

    integer :: i, curr, next, j
    real(kind=dp) :: u, cumulative

    ! Sample initial state by cumulative method
    call random_number(u)
    cumulative = 0.0_dp
    curr = n
    do j = 1, n
      cumulative = cumulative + init_prob(j)
      if (u < cumulative) then
        curr = j
        exit
      end if
    end do
    states(1) = curr
    obs(1) = means(curr) + sds(curr) * random_normal()

    ! Generate subsequent states and observations
    do i = 2, nobs
      call random_number(u)
      cumulative = 0.0_dp
      next = n
      do j = 1, n
        cumulative = cumulative + trans_mat(curr, j)
        if (u < cumulative) then
          next = j
          exit
        end if
      end do
      ! Fallback for rounding: if u not captured, assign last state
      if (u >= cumulative) next = n

      curr = next
      states(i) = curr
      obs(i) = means(curr) + sds(curr) * random_normal()
    end do

  end subroutine simulate_hmm

  pure function stationary(trans_mat, tol, max_iter) result(eq_dist)
    ! Computes the equilibrium (stationary) distribution for a Markov chain.
    ! It uses power iteration to find the eigenvector corresponding to eigenvalue 1,
    ! normalized so that sum(eq_dist) = 1.
    !
    ! Input:
    !   trans_mat - an N x N transition matrix (each row should sum to 1)
    !   tol       - (optional) tolerance for convergence (default 1.0e-8_dp)
    !   max_iter  - (optional) maximum number of iterations (default 10000)
    !
    ! Output:
    !   eq_dist   - the equilibrium distribution vector.
    
    implicit none
    real(kind=dp), intent(in) :: trans_mat(:,:)
    real(kind=dp), optional, intent(in) :: tol
    integer, optional, intent(in) :: max_iter
    real(kind=dp), allocatable :: eq_dist(:), eq_dist_old(:)
    integer :: n, iter, local_max_iter
    real(kind=dp) :: local_tol, diff, sum_eq

    ! Check that trans_mat is a square matrix
    if (size(trans_mat,1) /= size(trans_mat,2)) then
       error stop "Transition matrix must be square."
    endif

    ! Set default values for optional arguments
    if (present(tol)) then
       local_tol = tol
    else
       local_tol = 1.0e-8_dp
    endif

    if (present(max_iter)) then
       local_max_iter = max_iter
    else
       local_max_iter = 10000
    endif

    ! Determine the size of the matrix (it is square)
    n = size(trans_mat, 1)
    
    ! Allocate the probability vectors
    allocate(eq_dist(n), eq_dist_old(n))
    
    ! Initialize with equal probabilities
    eq_dist = 1.0_dp / real(n, dp)
    
    iter = 0
    do
       iter = iter + 1
       eq_dist_old = eq_dist
       
       ! Multiply the current distribution by the transition matrix
       eq_dist = matmul(eq_dist_old, trans_mat)
       
       ! Normalize so that the sum of entries equals 1
       sum_eq = sum(eq_dist)
       eq_dist = eq_dist / sum_eq
       
       ! Check convergence using the maximum absolute difference
       diff = maxval(abs(eq_dist - eq_dist_old))
       if (diff < local_tol) exit
       if (iter >= local_max_iter) exit
    end do

  end function stationary
end module hidden_markov_mod
