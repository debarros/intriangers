intriangers = function(a = 1, b = 1, degerenerates = F, max = 1000000) {
  output = integer(0)
  index = 1
  for (i in 1:max) {
    s1 = i
    s2 = i + a
    s3 = i + a + b
    if ((s1 + s2 > s3) | ((s1 + s2 == s3) & (degerenerates))){
      semi = (s1 + s2 + s3)/2
      Area = sqrt(semi * (semi - s1) * (semi - s2) * (semi - s3))
      if (!is.nan(Area)){
        if (Area == round(Area,0)) {
          output[index] = i
          index = index + 1
        }
      }
    }
  }
  return(output)
}
