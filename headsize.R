# estimate the surface of spherical phage head
# accessible surface area (ASA) is the surface area of a biomolecule that is accessible to a solvent.
# Once two molecules binding together, there will be an area on both molecules been folded. By comparing the HK97 data, ASA of total 420_mer
# is about the 95% of the 35-mer * 12
# the data are from HK97 (T = 7)
# parameters
# s_hep: ASA of heptamer
# s_35: ASA of 35_mer
# s_42: ASA of 42_mer
# s_inter: area of interface when two heptamer units binding
data <- read.csv("headsize_HK97.csv", header = T)
head(data)
s_35 =  data[, 3]

s_420 = s_35 *12*0.95

surface_area = mean(s_420)/2

