#FIXME: handle seed
runAlgoOnInstance = function(scen, instance, vals, seed = 123L, cutoff.time = 1L) {
  
  cmd = file.path(scen$aclib.dir, scen$algo)
  
  args = character(0)
  args[1L] = file.path(scen$aclib.dir, instance)
  args[2L] = 0
  args[3L] = scen$cutoff.time
  #FIXME: what it this?
  args[4L] = 2^31 - 1L
  args[5L] = seed
 
  vals = collapse(sprintf("-%s '%s'", names(vals), vals), " ")
  args = c(args, vals) 
  
  system3(cmd, args)
}

