#

#using Pkg; Pkg.add(url="https://github.com/aaowens/PSID.jl", rev="2021")
using PSID
cd("./psid/create/source")
psid_raw = makePSID("../class.json")
pwd()
using CSV

CSV.write("../psid_raw.csv", psid_raw)