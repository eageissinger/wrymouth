Subject: Stable isotope (d13C and d15N) and feeding preference of wrymouth (<i>Cryptacanthodes maculatus</i>) in eastern Maine, USA.

Data collected by: Emilie A. Geissinger, Brian F. Beal, and William G. Ambrose, Jr. Assistance was provided by Cody Jourdet (Downeast Institute) 
and Dr. Beverely Johnson and Phil Dostie (Bates College).

Background: Two intertidal mudflates in eastern Maine (Washington County) were sampled in this study: Larrabee Cove in Machiasport and 
Mud Hole Cove on Great Was Island, Beals. Wrymouth and potential prey were sampled in both mudflats in the spring and summer of 2013. 
Wrymouth were collected by walking on and around burrows to collapse them. Fish were collected when they surfaced (after 10-15 minutes), 
and immediately paralyzed in 1.0 g of Tricane-S dissolved in 1.0 liter of seawater (collection method approved by the Institutional Animal 
Care and Use Committee [IACUC] at University of Maine, Orono; Beal et al. 2016). Specimens were depurated for 24-48 hours and then stored 
at -10°C. Samples were freeze-dried prior to isotopic analysis. Algal tissue, muscle tissue from large animals, and the entire soft tissue 
of small animals were analyzed. Tissue samples weighed between 0.4-0.6 mg, and algal samples between 2.0-2.5 mg. Stable isotope analyses was 
performed using a Delta V stable isotope ratio mass spectrometer (IRMS) (Costech Elemental Analyzer Model 4010) in the Environmental Geochemistry 
Laboratory, Bates College, in Lewiston, Maine.  Stable isotopes were expressed in the δ notation.Individual wrymouth were weighed to the nearest 0.1 g, 
and total length measured to the nearest 0.5 cm. Full and empty stomachs were weighed, and the difference was used to calculate the mass of gut contents. 
Contents were observed under a stereoscopic microscope at 30x and individuals classified to the lowest possible taxonomic level and recorded as present, 
following methods of Beal et al. (2016). Statistical analysis was conducted in R. 

Format: all file formats are comma separted (.csv)

All R code is available at https://github.com/eageissinger/wrymouth and details provided in this file.

Data:

title: List of species sampled
file name: species_list.csv
date of data collection: 1-2 May 2013 and 21-22 August 2013
method of data collection: Species were collected from the intertidal mudflat. Wrymouth were collected by walking on and around burrows to collapse them. 
Fish were collected when they surfaced (after 10-15 minutes), and immediately paralyzed in 1.0 g of Tricane-S dissolved in 1.0 liter of seawater (collection 
method approved by the Institutional Animal Care and Use Committee [IACUC] at University of Maine, Orono; Beal et al. 2016). 
information on variables:
-year: year of collection
-month: month of collection
-day: day of collection
-site: collection site. LAR stands for Larrabee Cove, and MHC stands for Mud Hole Cove
-scientific_name: accepted scientific name
-common_name: accepted common name
-replicates: number of replicates collected
-notes: observations during collection.

title: Gut content data for wrymouth (<i>Cryptacanthodes maculatus</i>)
file name: gut_contents.csv
date of data collection: 1-2 May 2013 and 21-22 August 2013
method of data collection: Individual wrymouth were weighed to the nearest 0.1 g, and total length measured to the nearest 0.5 cm. Full and empty stomachs 
were weighed, and the difference was used to calculate the mass of gut contents. Contents were observed under a stereoscopic microscope at 30x and individuals 
classified to the lowest possible taxonomic level and recorded as present, following methods of Beal et al. (2016).
information on variables:
-year: year of collection
-month: month of collection
-day: day of collection
-site: collection site. LAR stands for Larrabee Cove, and MHC stands for Mud Hole Cove
-fish_number: number identifier assigned to each fish
-length_cm: Total length of wrymouth in centimeters
-weight_g: Total weight of wrymouth in grams
-sex: identified sex of fish M represents male, F represents female, blank represents unknown.
-WFG: Weight of full gut, in grams
-WEG: Weight of empty gut, in grams
-contents: list of species found in gut, identified to the nearest possible classification
-notes: observations during content analysis

title: d13C and d15N values of all species collected (see species_list.csv)
file name: wry_isotope_full.csv
date of data collection: samples were analyzed from November 2013 to March 2014.
method of data collection: Tissue samples weighed between 0.4-0.6 mg, and algal samples between 2.0-2.5 mg. Stable isotope analyses was performed using a 
Delta V stable isotope ratio mass spectrometer (IRMS) (Costech Elemental Analyzer Model 4010) in the Environmental Geochemistry Laboratory, Bates College, 
in Lewiston, Maine.  Stable isotopes were expressed in the δ notation according to the following definition:
δX(‰)=[(R_SAMPLE/R_STANDARD )-1]× 10^3, 	[Equation 1]
where X is 13C or 15N, R is 13C:12C or 15N:14N, and the standards were Vienna pee dee belemnite (VPDB) and air for carbon and nitrogen, respectively. 
Accuracy and precision of the IRMS was determined by multiple analyses of working standards (acetanilide, cod meat, and caffeine), run at the beginning, 
middle, and end of every 40 samples. 
information on variables:
-site: sample location, LAR is Larrabee Cove, MHC is Mud Hole Cove
-analysis: analysis identifier for the IRMS
-sampleID: unique ID for each sample for the IRMS
-kingdom: classification
-phylum: classification
-class: classifcation
-order: classification
-type: Species name
-name: short hand name
-fish_number: unique number identifier for wrymouth, only
-common_name: common name of species, when applicable
-mass_mg: mass of sample in miligrams
-%N: percent nitrogen
-umoles_N: micormole of nitrogen
-d15N: d15N (ppm) nitrogen
-%C: percent carbon
-umoles_C: micromole of carbon
-d13C: d13C (ppm) carbon
-C/N: carbon to nitrogen ratio
-date_run: date the sample was run in the IRMS
-season: season that sample was collected.

The following files are output from the above raw data, and are used for stable isotope analysis in R (siar).

title: wrymouth stable isotope signatures
file name: consumer.csv
date of data collection: 1-2 May 2013 and 21-22 August 2013
method of data collection: see species_list.csv and wry_isotopes_full.csv
information on variables:
-code: unique code identifier for siar analysis. 1 are samples from Larrabee Cove in the spring of 2013, 2 are samples from Larrabee Cove in the summer of 2013, 
3 are samples from Mud Hole Cove in the spring of 2013, and 4 are samples from Mud Hole Cove in the summer of 2013.
-d15N: delta15Nitrogen signatures
-d13C: delta13Carbon signatures

The final data file is an output from Rcode siar-analysis.R (see code description below):
title: HDRS output
file name: hrds.csv
date of data collection: NA
method of data collection: NA
information on variables:
-parameter: cluster group (see cluster-trophic-position.R)
-group: group of wrymouth. 1 are samples from Larrabee Cove in the spring of 2013, 2 are samples from Larrabee Cove in the summer of 2013, 
3 are samples from Mud Hole Cove in the spring of 2013, and 4 are samples from Mud Hole Cove in the summer of 2013.
-Low95hdr: lower 95% confidence interval
-High95hdr: upper 95% confidence interval
-mode: mode of proportion of diet
-mean: mean (average) of proportion of diet

R code:

data-format.R
This code is to format data for analysis. It is necessary to run this script before other scripts.

temporal-spatial-change.R
This code is to analyze the spatial and temporal change in wrymouth isotope signatures, and other available species
in the system.

gut-contents.R
This code is used to analyze changes in gut fullness across space and time for wrymouth.

cluster-trophic-position.R
This code uses heirarchical cluster analysis for each food web, and also calcuates trophic position for each food web.

siar-analysis.R
This code uses output from cluster-trophic-position.R to determine diet proportions for wrymouth across two locations and seasons.

maine-map.R
This is the code for the study site map. This code has no dependencies on other code.

figures.R
This is the code for all figures for our project. Analyses must be conducted prior to running this code (temporal-spatial-change.R,
gut-contents.R, cluster-trophic-position.R, siar-analysis.R).


