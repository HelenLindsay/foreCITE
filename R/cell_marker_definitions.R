# Maecker and McCoy https://www.nature.com/articles/nri3158 ----
maecker_gates = list(
    tcell = "CD3+",

    CD4_tcell = c("CD3+", "CD4+"),
    CD8_tcell = c("CD3+", "CD4+"),

    naive_CD4_tcell = c("CD3+", "CD4+","CCR7+","CD45RA+"),
    memory_CD4_tcell = c("CD3+", "CD4+","CCR7+","CD45RA-"),
    effector_CD4_tcell = c("CD3+", "CD4+","CCR7-","CD45RA+"),
    effector_memory_CD4_tcell = c("CD3+", "CD4+","CCR7-","CD45RA-"),
    naive_CD8_tcell = c("CD3+", "CD8+","CCR7+","CD45RA+"),
    memory_CD8_tcell = c("CD3+", "CD8+","CCR7+","CD45RA-"),
    effector_CD8_tcell = c("CD3+", "CD8+","CCR7-","CD45RA+"),
    effector_memory_CD8_tcell = c("CD3+", "CD8+","CCR7-","CD45RA-"),

    activated_naive_CD4_tcell = c("CD3+", "CD4+","CCR7+","CD45RA+", 
                                  "CD38+", "HLA-DR+"),
    activated_memory_CD4_tcell = c("CD3+", "CD4+","CCR7+","CD45RA-",
                               "CD38+", "HLA-DR+"),
    activated_effector_CD4_tcell = c("CD3+", "CD4+","CCR7-","CD45RA+",
                                     "CD38+", "HLA-DR+"),
    activated_effector_memory_CD4_tcell = c("CD3+", "CD4+","CCR7-","CD45RA-",
                                            "CD38+", "HLA-DR+"),
    activated_naive_CD8_tcell = c("CD3+", "CD8+","CCR7+","CD45RA+",
                                  "CD38+", "HLA-DR+"),
    activated_memory_CD8_tcell = c("CD3+", "CD8+","CCR7+","CD45RA-",
                                   "CD38+", "HLA-DR+"),
    activated_effector_CD8_tcell = c("CD3+", "CD8+","CCR7-","CD45RA+",
                                     "CD38+", "HLA-DR+"),
    activated_effector_memory_CD8_tcell = c("CD3+", "CD8+","CCR7-","CD45RA-",
                                            "CD38+", "HLA-DR+"),

    t_helper_1_cell = c("CD3+", "CD4+","CCR7+","CD45RA+", "CXCR3+", "CCR6-"),
    t_helper_17_cell = c("CD3+", "CD4+","CCR7+","CD45RA+", "CXCR3-", "CCR6+"),
    t_helper2_cell = c("CD3+", "CD4+","CCR7+","CD45RA+", "CXCR3-", "CCR6-"),

    treg_cell = c("CD3+", "CD4+", "CCR4+", "CD25+", "CD127low"),
    memory_treg_cell = c("CD3+", "CD4+", "CCR4+", "CD25+",
                         "CD127low", "CD45RO+"),
    naive_treg_cell = c("CD3+", "CD4+", "CCR4+", "CD25+",
                        "CD127low", "CD45RO-"),
    activated_treg_cell = c("CD3+", "CD4+", "CCR4+", "CD25+",
                            "CD127low", "HLA-DR+"),

    bcell = c("CD3-", "CD19+"),
    naive_bcell = c("CD3-", "CD19+", "CD27-"),
    memory_bcell = c("CD3-", "CD19+", "CD27+"),
    permablast = c("CD3-", "CD19+", "CD27+", "CD20-", "CD38+"),
    transitional_bcell = c("CD3-", "CD19+", "CD24high", "CD38high"),

    monocytes = c("CD3-", "CD19-", "CD14+"),
    CD16_monocyte = c("CD3-", "CD19-", "CD14+", "CD16+"),
    classical_monocyte = c("CD3-", "CD19-", "CD14+", "CD16-"),
    myeloid_dendritic_cell = c("CD3-", "CD19-", "CD14-", "CD20-",
                               "HLA-DR+", "CD11c+"),
    plasmacytoid_dentritic_cell =  c("CD3-", "CD19-", "CD14-", "CD20-",
                                     "HLA-DR+", "CD123+"),
    nk_cell =  c("CD3-", "CD19-", "CD14-", "CD20-", "CD16+")
)

# Cell Singaling Technologies ----
#https://www.cellsignal.com/pathways/immune-cell-markers-human
cell_signaling_technologies <- list(
    leukocyte = c("CD45+"),
    myeloid_cell = c("CD45+", "CD11b+"),

    # Granulocytes
    
    # These are alternative definitions (Need to be able to code and/or)
    neutrophil_1 = c("CD45+", "CD11b+", "CD16+", "CD66b+"),
    neutrophil_2 = c("CD45+", "CD11b+", "CD15+"),
    
    eosinophil = c("CD45+", "CD11b+", "CD193+", "Siglec-8+", "CD16-"),
    basophil = c("CD45+", "CD11b+", "FCER1A+", "CD117-"),
    activated_basophil = c("CD45+", "CD11b+", "FCER1A+", "CD117-",
                           "CD63+", "CD203+"),
    mast_cell = c("CD45+", "CD11b+", "FCER1A+", "CD117+", "Tryptase+"),
    activated_mast_cell = c("CD45+", "CD11b+", "FCER1A+", "CD117+",
                            "Tryptase+", "CD203+"),
    
    # Myeloid-derived supressor cells 
    
    monocytic_myeloid_derived_supressor_cell = c("CD45+", "CD11b+",
        "CD15-", "CD14+", "HLA-DR-"),
    
    polymorphonuclear_myeloid_derived_supressor_cell = c("CD45+", "CD11b+",
        "CD15+", "CD14-", "HLA-DR-"),
    
    # Plasmacytoid dendritic cells
    
    plasmacytoid_dentritic_cell = c("CD45+", "CD11b+", "HLA-DR+", "CD123+"),
    activated_plasmacytoid_dentritic_cell = c("CD45+", "CD11b+",
        "HLA-DR+", "CD123+", "CD83+"),
    
    
    # Conventional dentritic cells 
    
    conventional_dendritic_cell = c("CD45+", "CD11b+", "CD11c+", "HLA-DR+"),
    
    # Alternative definitions for cDC1:
    conventional_dendritic_cell1_1 = c("CD45+", "CD11b+", "CD11c+", "HLA-DR+",
                                       "XCR1"),
    conventional_dendritic_cell1_2 = c("CD45+", "CD11b+", "CD11c+", "HLA-DR+",
                                       "CLEC9A+"),
    
    # Alternative definitions for cDC2
    conventional_dendritic_cell2_1 = c("CD45+", "CD11b+", "CD1c+"),
    conventional_dendritic_cell2_1=2 = c("CD45+", "CD11b+", "SIRPa+"),
    
    activated_conventional_dentritic_cell =  c("CD45+", "CD11b+", "CD83+"),
    
    # Macrophages
    macrophage =  c("CD45+", "CD11b+", "CD68+", "HLA-DR+", "CD11c-"),
    
    # Alternative definitions
    m1_like_macrophage_1 = c("CD45+", "CD11b+", "CD86+"),
    m1_like_macrophage_2 = c("CD45+", "CD11b+", "CD80+"),
    m1_like_macrophage_2 = c("CD45+", "CD11b+", "iNOS+"),
    
    # Alternative definitions
    m2_like_macrophage_1 = c("CD45+", "CD11b+", "CD163+"),
    m2_like_macrophage_2 = c("CD45+", "CD11b+", "CD206+"),
    
    # T cells
    tcell = c("CD45+", "CD3+"),
    
    # Not how to code Granzyme + or Perforin+
    cytotoxic_tcell = c("CD45+", "CD3+", "CD8+"), 
    
    helper_tcell = c("CD45+", "CD3+", "CD4+"),
    
    t_helper_9_cell =  c("CD45+", "CD3+", "CD4+", "PU.1+", "IL-9+"),
    t_helper_1_cell =  c("CD45+", "CD3+", "CD4+", "T-Bet+", "IFNgamma+"),
    t_helper_2_cell =  c("CD45+", "CD3+", "CD4+", "GATA-3+", "IL-4+"),
    t_helper_22_cell =  c("CD45+", "CD3+", "CD4+", "AHR+", "IL-22+"),
    t_helper_17_cell =  c("CD45+", "CD3+", "CD4+", "RORgammat+", "IL-17+"),
    t_follicular_helper_cell =  c("CD45+", "CD3+", "CD4+",
                                  "Bcl-6+", "CXCR5+", "IL-21+"),
    regulatory_tcell =  c("CD45+", "CD3+", "CD4+", "FoxP3+", "CD25+"),
    
    # T cell functional states
    
    # Alternative markers
    activated_tcell_1 =  c("CD45+", "CD3+", "CD69+"),
    activated_tcell_2 =  c("CD45+", "CD3+", "CD25+"),
    
    # Alternative markers
    naive_tcell_1 =  c("CD45+", "CD3+", "CD45RA+", "CD62L+"),
    naive_tcell_2 =  c("CD45+", "CD3+", "CD45RA+", "CCR7+"),
    
    # Alternative markers
    effector_memory_tcell_1 =  c("CD45+", "CD3+", "CD45RO+", "CD62L-"),
    effector_memory_tcell_2 =  c("CD45+", "CD3+", "CD45RO+", "CCR7-"),
    
    # Alternative markers
    central_memory_tcell_1 =  c("CD45+", "CD3+", "CD45RO+", "CD62L+"),
    central_memory_tcell_2 =  c("CD45+", "CD3+", "CD45RO+", "CCR7+"),
    
    # Alternative markers
    effector_tcell_1 = c("CD45+", "CD3+", "CD45RA+", "CD62L-"),
    effector_tcell_2 = c("CD45+", "CD3+", "CD45RA+", "CCR7-"),
        
    progenitor_exhausted_tcell =  c("CD45+", "CD3+", "TCF1/TCF7+",
                                    "PD-1high", "TIM-3high"),
    terminally_exhausted_tcell = c("CD45+", "CD3+","Tox/Tox2+",
                                   "PD-1high", "TIGIT+"),
    
    # Natural killer t-cells
    
    natural_killer_tcell = c("CD45+", "CD56+", "CD3+"),
    
    # Also called Type I NK t-cell
    invariant_natural_killer_tcell = c("CD45+", "CD56+", "CD3+",
                                       "TCRVa24Vb11+"),
    # Type II has no specific marker
    
    # Natural killer cells
    
    natural_killer_cell =  c("CD45+", "CD56+", "CD3-"),
    peripheral_blood_cytotoxic_natural_killer_cell =
        c("CD45+", "CD56+", "CD3-", "CD56low", "CD16+"),
        # Not sure how to code Granzyme+ / Perforin+
    
    # Different populations with same marker signature?
    peripheral_blood_immature_natural_killer_cell =
        c("CD45+", "CD56+", "CD3-", "CD56high", "CD16-", "NCR+"),
    peripheral_blood_immature_regulatory_killer_cell =
        c("CD45+", "CD56+", "CD3-", "CD56high", "CD16-", "NCR+"),
    
    activated_natural_killer_cell =  c("CD45+", "CD56+", "CD3-", "CD69+"),
    
    # B cells 
    bcell = c("CD45+", "CD19+"), 
    naive_bcell = c("CD45+", "CD19+", "IgD+", "CD27-"),
    switched_memory_bcell = c("CD45+", "CD19+", "IgD-", "CD27+"), 
    unswitched_memory_bcell = c("CD45+", "CD19+", "IgD+", "CD27+"), 
    
    # Plasma cells (alterntive definition)
    plasma_cell_1 = c("CD45+", "BCMA+"),
    plasma_cell_2 = c("CD45+", "CD138+")
)