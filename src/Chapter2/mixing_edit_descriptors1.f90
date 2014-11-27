program mixing_edit_descriptors1
  implicit none
  logical :: convergenceStat = .true.

  print '(a, 2x, a, 4/, a, l1)', &
       "Simulation", "finished.", &
       "Convergence status = ", &
       convergenceStat
end program mixing_edit_descriptors1
