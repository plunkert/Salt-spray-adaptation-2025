# Overview
This repository contains data, R scripts, figures, and tables associated with a study of salt spray exclusion and tolerance strategies in locally adapted coastal yellow monkeyflower.

If used, please cite as: Plunkert M., Durant P.C., Egeler K., and Lowry, D. 2025. bioRxiv. Foliar salt spray exclusion and tissue tolerance underlie local adaptation to oceanic salt spray

# Scripts

accession_map.R creates a map of coastal and inland accessions used in this study

leaf_surface_trait_analysis_clean.R takes data from leaf_area_data.xlsx, processed_baseline_gsw.xlsx, stomata_lengths_leaf_surface_data.xlsx, 
stomatal density data.xlsx, and Contact Angle Measurements.xlsx to compare leaf surface traits (hydrophobicity, stomatal traits, succulence) between
coastal and inland accessions grown in a common garden. These data all come from common garden experiments where no salt spray treatments were applied.

leaf_disc_necrosis_date_analysis.R tests for evidence of tissue tolerance from a leaf disk assay. It takes data from Leaf_disc_date_data.xlsx 
to compare time to the start of tissue necrosis and time to full tissue necrosis between leaf disks of coastal and inland accessions plated on media with and without NaCl. 



# Data

Contact Angle Measurements.xlsx:            contact angle and image file tracking for assaying leaf wettability

leaf_area_data.xlsx:                        leaf areas for leaves used in succulence, LMA, and leaf water drop adhesion assay

Leaf_disc_date_data.xlsx:                   Tissue tolerance test. Days to tissue necrosis (beginning of tissue necrosis and complete necrosis) on plates with and without NaCl.

leaf_surface_data.xlsx:                     records which leaf was collected for stomatal and leaf hydrophobicity traits, and leaf masses

MP-AES_leaf_mass_area.xlsx                  leaf mass and area for leaves that were sprayed with NaCl solution or DI water and sodium levels quantified with MP-AES

processed_baseline_gsw.xlsx:                stomatal conductance data (from a common garden that was a separate batch of plants from hydrophobicity and other stomatal traits). 

stomata_lengths_leaf_surface_data.xlsx:     stomatal length from common garden

stomatal density data.xlsx:                  stomatal density from common garden 



