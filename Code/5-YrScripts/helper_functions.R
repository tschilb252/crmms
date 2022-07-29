## Functions

# standard to metric conversion
af_to_cm <- function(af) {
  cm = af * 1233.48185532
  return(cm)
}

kaf_to_mcm <- function(kaf) {
  mcm = af_to_cm(kaf) / 1000
  return(mcm)
}

maf_to_bcm <- function(maf) {
  bcm = af_to_cm(maf) / 10^3
  return(bcm)
}

ft_to_m <- function(ft) {
  ft*0.3048
}
