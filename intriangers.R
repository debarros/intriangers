# Choose intervals for the differences between the lengths of the sides of a triangle.
# Find triangles where the side lengths follow that pattern and where the area of the triangle is also an integer.
# Return the length of the shortest side.
# Example:
# intriangers(1,1)
# This returns a vector where the first element is 3, indicating that there is a triangle with side lengths 3, 3+1, and 3+1+1 whose area is a whole number.
# The second element in the return is 13, referring to the triangle with side lengths 13, 14, and 15, whose area is 84.
# intriangers(2,7)
# This returns a vector where the first element is 11, referring to the triangle with side lengths 11, 11+2, 11+2+7, whose area is 66.
# The degenerates flag can be set to allow triangles with 0 area.
# The max parameter can be changed to alter the max length of the shortest side of the triangle.
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
