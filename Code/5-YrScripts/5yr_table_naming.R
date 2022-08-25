# variable names for 5-yr Table

coorOps_Mead_slots <- c( 
  "LBSurplusConditions" = "Surplus.AnnualSurplusFlag", 
  "AnnualFloodControlFlag" = "CoordinatedOperationsFlagsForFreqTbl.AnnualFloodControlFlag",
  "LBNormalCondition" = "CoordinatedOperationsFlagsForFreqTbl.LBNormalCondition",
  "DCP1110" = "DCP BWSCP Flags.DCPBWSCP1110",
  "DCP1090" = "DCP BWSCP Flags.DCPBWSCP1090",
  "LBShortage" = "CoordinatedOperationsFlagsForFreqTbl.AnnualShortageAnyTypeFlag",
  "LBShortageStep1" = "CoordinatedOperationsFlagsForFreqTbl.Step1ShortageFlag",
  "DCP1075" = "DCP BWSCP Flags.DCPBWSCP1075",
  "LBShortageStep2" = "CoordinatedOperationsFlagsForFreqTbl.Step2ShortageFlag",
  "DCP1050" = "DCP BWSCP Flags.DCPBWSCP1050",
  "DCP1045" = "DCP BWSCP Flags.DCPBWSCP1045",
  "DCP1040" = "DCP BWSCP Flags.DCPBWSCP1040",
  "DCP1035" = "DCP BWSCP Flags.DCPBWSCP1035",
  "DCP1030" = "DCP BWSCP Flags.DCPBWSCP1030",
  "LBShortageStep3" = "CoordinatedOperationsFlagsForFreqTbl.Step3ShortageFlag",
  "DCP1025" = "DCP BWSCP Flags.DCPBWSCP1025"
)

coorOps_Powell_slots <- c( 
  "EqualizationAbove823" = "CoordinatedOperationsFlagsForFreqTbl.EqualizationAbove823",
  "EqualizationAt823" = "CoordinatedOperationsFlagsForFreqTbl.EqualizationAt823",
  
  "UpperBalancingAbove823" = "CoordinatedOperationsFlagsForFreqTbl.UpperBalancingAbove823",
  "UpperBalancingAt823" = "CoordinatedOperationsFlagsForFreqTbl.UpperBalancingAt823",
  "UpperBalancingBelow823" = "CoordinatedOperationsFlagsForFreqTbl.UpperBalancingBelow823",
  
  "MidElevationReleaseAt823" = "CoordinatedOperationsFlagsForFreqTbl.MidElevationReleaseAt823",
  "MidElevationReleaseAt748" = "CoordinatedOperationsFlagsForFreqTbl.MidElevationReleaseAt748",
  
  "LowerBalancingAbove823" = "CoordinatedOperationsFlagsForFreqTbl.LowerBalancingAbove823",
  "LowerBalancingAt823" = "CoordinatedOperationsFlagsForFreqTbl.LowerBalancingAt823",
  "LowerBalancingBelow823" =  "CoordinatedOperationsFlagsForFreqTbl.LowerBalancingBelow823"
)


# Table Naming Output
coorOps_Mead_out <- c( 
  "LBSurplusConditions" = "Surplus Condition - any amount (Mead>= 1,145 ft)", 
  "AnnualFloodControlFlag" = "Surplus - Flood Control",
  "LBNormalCondition" = "Normal Year or ICS Surplus Condition (Mead < 1,145 and > 1,075 ft)",
  "DCP1110" = "Recovery of DCP ICS / Mexico's Water Savings (Mead >/>= 1,110 ft)",
  "DCP1090" = "DCP Contribution / Mexico's Water Savings (Mead <= 1,090 and > 1,075 ft)",
  "LBShortage" = "Shortage Condition - any amount (Mead <= 1,075 ft)",
  "LBShortageStep1" = "Shortage / Reduction - 1st Level (Mead <= 1,075 and >= 1,050 ft)",
  "DCP1075" = "DCP Contribution / Mexico's Water Savings (Mead <= 1,075 and >= 1,050 ft)",
  "LBShortageStep2" = "Shortage / Reduction - 2nd Level (Mead < 1,050 and >= 1,025 ft)",
  "DCP1050" = "DCP Contribution / Mexico's Water Savings (Mead < 1,050 and > 1,045 ft)",
  "DCP1045" = "DCP Contribution / Mexico's Water Savings (Mead <= 1,045 and > 1,040 ft)",
  "DCP1040" = "DCP Contribution / Mexico's Water Savings (Mead <= 1,040 and > 1,035 ft)",
  "DCP1035" = "DCP Contribution / Mexico's Water Savings (Mead <= 1,035 and > 1,030 ft)",
  "DCP1030" = "DCP Contribution / Mexico's Water Savings (Mead <= 1,030 and >=/> 1,025 ft)",
  "LBShortageStep3" = "Shortage / Reduction - 3rd Level (Mead < 1,025 ft)",
  "DCP1025" = "DCP Contribution / Mexico's Water Savings (Mead </<= 1,025 ft)"
)

coorOps_Powell_out <- c( 
  "Equalization" = "Equalization Tier (Powell >= Equalization [EQ] Elevation)",
  "EqualizationAbove823" = "Equalization - annual release > 8.23 maf",
  "EqualizationAt823" = 'Equalization - annual release = 8.23 maf',
  "UpperBalancing" = "Upper Elevation Balancing Tier (Powell < EQ Elevation and >= 3,575 ft)",
  "UpperBalancingAbove823" = 'Upper Elevation Balancing - annual release > 8.23 maf',
  "UpperBalancingAt823" = "Upper Elevation Balancing - annual release = 8.23 maf",
  "UpperBalancingBelow823" = "Upper Elevation Balancing - annual release < 8.23 maf",
  "MidElevationRelease" = "Mid-Elevation Release Tier (Powell < 3,575 and >= 3,525 ft)",
  "MidElevationReleaseAt823" = "Mid-Elevation Release - annual release = 8.23 maf",
  "MidElevationReleaseAt748" = "Mid-Elevation Release - annual release = 7.48 maf",
  "LowerBalancing" = "Lower Elevation Balancing (Powell < 3,525 ft)",
  "LowerBalancingAbove823" = "Lower Elevation Balancing - annual release > 8.23 maf",
  "LowerBalancingAt823" = "Lower Elevation Balancing - annual release = 8.23 maf",
  "LowerBalancingBelow823" =  "Lower Elevation Balancing - annual release < 8.23 maf"
)
