module Rbase

using RCall

include("precompile.jl")


# load R functions
R"""
source("src/make_gridFractArea.R")
"loaded"
"""


function get_ChinaRegionalMean(fs)
  
end

end # module Rbase
