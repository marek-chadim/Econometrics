#

using DataFrames
using CSV
cd("./psid/create")
raw = CSV.read("psid_raw.csv", DataFrame, copycols = true);

println(names(raw))

# We have two variables for labor income due to some change in definition in 1993. Get rid of break and put income in one variable.
### !!!!!!!!!!!!!!
# If doing serious stuff, double check how bad the inconsistency is!!!
inds = (raw.year .<= 1993) .& (raw.ishead .== true)
raw.labor_inc_spouse[inds] .= raw.labor_inc_pre_spouse[inds]
inds = (raw.year .<= 1993) .& (raw.ishead .== false)
raw.labor_inc_ind[inds] .= raw.labor_inc_pre_ind[inds]
raw = raw[:, Not(:labor_inc_pre_ind)]


raw = raw[raw.:famid_1968 .< 3000,:] # Keep only SRC sample


## The PSID didn't start asking the sex of the spouse until 2015 (It was assumed female)
unique(raw[:,[:sex_ind_label]])
# Female if missing
raw.sex_ind_label = coalesce.(raw.sex_ind_label, "Female") 
# rename sex
unique(raw[:,:sex_ind_label])
rename!(raw, :sex_ind_label => :sex)

# family id is integer
raw[!,:id_family] = convert.(Int64,raw[:,:id_family])

# Replace missing labor income with 0s. Not an innocent change, but probably less bad than dropping them. Depends on application
raw.labor_inc_ind = coalesce.(raw.labor_inc_ind, 0.0)

# rename labor income
rename!(raw, :labor_inc_ind => :labor_inc)

# condense info regarding head or spouse
println(unique(raw[:,[:rel_head_ind_label,:rel_head_ind]]))
raw[!,:rel_head] .= ""
raw[raw.rel_head_ind.==1.0 .|| raw.rel_head_ind.==10.0,:rel_head] .= "head"
raw[in.(raw.rel_head_ind, Ref([2.0, 20.0, 22.0])),:rel_head] .= "spouse"
raw = raw[:,Not(:rel_head_ind_label,:rel_head_ind)]

# make education an integer
println(unique(raw[:,:educ_ind]))
raw[!,:educ] = missings(Int64, size(raw,1))
raw[.!ismissing.(raw.educ_ind),:educ] = convert.(Int64,raw[.!ismissing.(raw.educ_ind),:educ_ind])
raw = raw[:,Not(:educ_ind,:educ_ind_code_ind)]

# make state an integer
println(unique(raw[:,:state_family]))
raw[!,:state] = missings(Int64, size(raw,1))
raw[.!ismissing.(raw.state_family),:state] = convert.(Int64,raw[.!ismissing.(raw.state_family),:state_family])
raw = raw[:,Not(:state_family,:state_family_code_fam)]

# make age an integer
println(unique(raw[:,:age_ind]))
raw[!,:age] = missings(Int64, size(raw,1))
raw[.!ismissing.(raw.age_ind),:age] = convert.(Int64,raw[.!ismissing.(raw.age_ind),:age_ind])
raw = raw[:,Not(:age_ind,:age_ind_code_fam)]

# make age of spouse an integer
println(unique(raw[:,:age_spouse]))
raw[!,:age_spouse2] = missings(Int64, size(raw,1))
raw[.!ismissing.(raw.age_spouse),:age_spouse2] = convert.(Int64,raw[.!ismissing.(raw.age_spouse),:age_spouse])
raw = raw[:,Not(:age_spouse,:age_spouse_code_fam)]
rename!(raw, :age_spouse2 => :age_spouse)


# condense info on marital status
marital_stats = unique(raw[:,:marital_status_ind_label])
println(marital_stats)
new_marital_stats = ["Married or perm. cohabiting", "Divorced", "Widow", "Single", "Separated", "Married, spouse absent"]
rename!(raw, :marital_status_ind_label => :marital_status)
for i in 1:6
    raw.marital_status[coalesce.(raw.marital_status .== marital_stats[i],false)] .= new_marital_stats[i]
end


# condense info on race
races = collect(skipmissing(unique(raw[:,:race_ind_label])))
println(races)
new_races = ["White", "Black", "Latino or Native American", "Other" , "Asian, Pacific Islander", "Latino origin or Pacific Islander", "Color besides black or white"]
raw[!,:race] = missings(String, size(raw,1))
for i in 1:7
    raw.race[coalesce.(raw.race_ind_label .== races[i],false)] .= new_races[i]
end
raw = raw[:,Not(:race_ind_label,:race_ind)]

# make number of kids an integer
println(unique(raw[:,:num_kids_family]))
raw[!,:num_kids] = missings(Int64, size(raw,1))
raw[.!ismissing.(raw.num_kids_family),:num_kids] = convert.(Int64,raw[.!ismissing.(raw.num_kids_family),:num_kids_family])

# expenditure is sum of expenditure on 
# food, health care, housing, trasport, childcare and education 
raw[!,:expenditure_family] .=  raw[:,:food_family].+raw[:,:health_family].+raw[:,:housing_family].+raw[:,:transport_family].+raw[:,:childcare_family].+raw[:,:educexp_family]

# condense info on household composition changes
display(unique(raw[:,[:family_comp_change_family_label,:family_comp_change_family]]))
raw[!,:family_comp_changed] = missings(Bool, size(raw,1))
raw[.!ismissing.(raw.family_comp_change_family),:family_comp_changed] .= true
raw[coalesce.(raw.family_comp_change_family .== 0.0, false),:family_comp_changed] .= false


rename!(raw, :transfer_inc_family => :transfer_inc)

raw = raw[:,[:id_ind,:famid_1968,:year, :id_family, :rel_head, :marital_status, :family_comp_changed, :sex, :educ, :state, :age, :age_spouse, :race, :num_kids, :labor_inc, :labor_inc_spouse, :expenditure_family, :net_wealth_family,:transfer_inc]]

raw

# load CPI data
cpi = CSV.read("cpi.csv", DataFrame, copycols = true);
cpi.DATE = parse.(Int64,last.(cpi.DATE,4))

# make relevant variables real (on 2015 basis)
for var in ["labor_inc", "labor_inc_spouse", "expenditure_family", "net_wealth_family", "transfer_inc"]
    for y in unique(raw[:,"year"])
        raw[raw.year .== y,var] = raw[raw.year .== y,var] ./ cpi.CPI[cpi.DATE .== y][1] .* 100
    end
end

CSV.write("psid_final.csv", raw)


