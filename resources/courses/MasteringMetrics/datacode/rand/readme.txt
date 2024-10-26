Mastering Metrics Replication Code
Readme for Tables 1.3 and 1.4 Code

Created by Hellary Zhang, uploaded on 3/29/2021

table13_rev.do: Replicates Table 1.3
	- Input dataset: /RAND/Data/rand_initial_sample_2.dta - This dataset is a subset of the file rand_initial_sample.dta found in the replication archive of the Aron-Dine et al. paper (2013) and contains demographic information, as well as health variables at baseline and exit. Each observation is at the individual level.
	- Table sample: The sample ultimately used to construct Table 1.3 consists of adult participants who are age 14 to 61 at the time of enrollment and who did not initially refuse enrollment in a plan (refused == 0). This sample is in line with the sample studied in Brook et al. (1983).
	- Output: table13_rev.doc

table14a_rev.do: Replicates Panel A of Table 1.4
	- Input dataset: 
		- /RAND/Data/person_years.dta - This dataset comes from the replication of the Aron-Dine et al. paper (2013) and consists of administrative claims data. Each observation is at the individual-year level. 
		- /RAND/Data/annual_spend.dta
	- Table sample: The sample used to construct Panel A of Table 1.4 consists of all participants (regardless of age at enrollment) who did not initially refuse enrollment in a plan (refused == 0). Note that this sample is slightly larger than the sample used in both Table 1.3 and Panel B of Table 1.4, because there is no restriction on age. This sample is in line with the 5,811 individuals outlined in Table 1 of the Aron-Dine et al. paper. 
	- Output: table14a_rev.doc

table14b_rev.do: Replicates Panel B of Table 1.4
	- Input dataset: /RAND/Data/rand_initial_sample_2.dta - This dataset is a subset of the file rand_initial_sample.dta found in the replication archive of the Aron-Dine et al. paper (2013) and contains demographic information, as well as health variables at baseline and exit. Each observation is at the individual level.
	- Table sample: The sample ultimately used to construct Panel B of Table 1.4 consists of adult participants who are age 14 to 61 at the time of enrollment and who did not initially refuse enrollment in a plan (refused == 0). This sample is in line with the sample studied in Brook et al. (1983). The sample is the same as that used in Table 1.3. 
	- Output: table14b_rev.doc


References:
Robert H. Brook et al., “Does Free Care Improve Adults’ Health? Results from a Randomized Controlled Trial,” New England Journal of Medicine, vol. 309, no. 23, December 8, 1983, pages 1426-1434
Aviva Aron-Dine, Liran Einav, and Amy Finkelstein, “The RAND Health Insurance Experiment, Three Decades Later,” Journal of Economic Perspectives, vol. 27, Winter 2013, pages 197-222. Replication archive: https://www.openicpsr.org/openicpsr/project/113919/version/V1/view
