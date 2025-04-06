program xhidden_markov
  ! simulate from Hidden Markov Model and print statistics on observations
  use kind_mod, only: dp
  use random_mod, only: random_seed_init
  use basic_stats_mod, only: mean, sd, print_basic_stats
  use hidden_markov_mod, only: simulate_hmm, stationary
  implicit none
  integer, parameter :: n = 3, nobs = 10**6
  integer :: i, ios, states(nobs)
  real(kind=dp) :: obs(nobs)
  real(kind=dp) :: freq(n), emp_trans(n,n), freq_theory(n)
  character(len=100) :: filename
  logical, parameter :: write_obs = .false.
  ! Initialize HMM parameters
  real(kind=dp), parameter :: init_prob(n) = [0.5_dp, 0.3_dp, 0.2_dp], &
     trans_mat(n,n) = reshape([0.7_dp,0.2_dp,0.3_dp, &
                               0.1_dp,0.5_dp,0.2_dp, &
                               0.2_dp,0.3_dp,0.5_dp], shape(trans_mat)), &
     means(n) = [3.0_dp, 0.0_dp, -2.0_dp], sds(n)   = [1.0_dp, 3.0_dp, 10.0_dp]
  print "('#obs: ', i0)", nobs

  ! Initialize RNG
  call random_seed_init(1234, nburn=1000, print_seed=.false.)

  ! Simulate HMM
  call simulate_hmm(n, init_prob, trans_mat, means, sds, nobs, states, obs)

  ! Write results to CSV
  filename = "out.csv"
  if (write_obs) then
     open(unit=10, file=filename, status="replace", action="write", iostat=ios)
     if (ios /= 0) then
       print *, "Error opening file ", trim(filename)
       stop
     end if
     write(10, "(A)") "obs_num,state,observation"
     do i = 1, nobs
       write(10, "(I8,1X,I3,1X,F10.6)") i, states(i), obs(i)
     end do
     close(10)
  end if

  print "(/,'State parameters:', /, 9x, *(a12))", "mean", "sd"
  do i=1,n
    print "(A,I0,2X,2F12.4)", "State ", i, means(i), sds(i)
  end do

  freq_theory = stationary(trans_mat)
  ! Compute and print state frequencies
  freq = 0.0_dp
  do i = 1, nobs
    freq(states(i)) = freq(states(i)) + 1.0_dp
  end do
  freq = freq/nobs
  print "(/,a)", "State frequencies:"
  print "(9x, *(a12))", "empirical", "theoretical", "diff"
  do i = 1, n
    print "(A,I0,2X,3F12.4)", "State ", i, freq(i), freq_theory(i), freq(i) - freq_theory(i)
  end do

  ! Compute and print empirical transition probabilities
  emp_trans = 0.0_dp
  do i = 1, nobs-1
    emp_trans(states(i), states(i+1)) = emp_trans(states(i), states(i+1)) + 1.0_dp
  end do
  do i = 1, n
    if (sum(emp_trans(i,:)) > 0.0_dp) then
      emp_trans(i,:) = emp_trans(i,:) / sum(emp_trans(i,:))
    end if
  end do
  print "(/,a)", "Theoretical transition probabilities:"
  do i = 1, n
    write(*, "(A,I0,A,2X,*(F8.4))") "From state ", i, ":", trans_mat(i,:)
  end do
  print "(/,a)", "Empirical transition probabilities:"
  do i = 1, n
    write(*, "(A,I0,A,2X,*(F8.4))") "From state ", i, ":", emp_trans(i,:)
  end do
  print "(/, a, f8.4)", "max absolute deviation: ", maxval(abs(trans_mat-emp_trans))
  call print_basic_stats(obs, fmt_header="(/,'observation stats')")

end program xhidden_markov