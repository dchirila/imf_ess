program character_type_examples
  implicit none

  ! given two source string-variables:
  character(len=4) :: firstName = "John"
  character(len=7) :: secondName = "Johnson"
  ! and 3 target variables (of different lengths):
  character(len=13) :: exactFit
  character(len=10) :: shorter
  character(len=40) :: wider =&
       "Some phrase to initialize this variable."

  ! below, we concatenate 'firstName' and 'secondName',
  ! assigning the result to strings of different sizes.
  ! note: '|'-characters serve as markers, to highlight
  ! the spaces in the actual output.

  ! expression fits exactly into 'exactFit'
  exactFit = firstName // ", " // secondName
  print*, "|", exactFit, "|"

  ! expression does not fit into 'shorter', so some
  ! characters at the end are truncated
  shorter = firstName // ", " // secondName
  print*, "|", shorter, "|"

  ! expression takes less space than available in
  ! 'wider', so whitespace is added as padding on
  ! the right (previous content discarded)
  wider = firstName // ", " // secondName
  print*, "|", wider, "|"
end program character_type_examples
