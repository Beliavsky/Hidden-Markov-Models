module hidden_markov_mod
  use kind_mod, only: dp
  use random_mod, only: random_normal
  implicit none
  private
  public :: simulate_hmm

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

end module hidden_markov_mod
