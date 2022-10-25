
tcell = "CD3+"

CD4_tcell = c("CD3+", "CD4+")
CD8_tcell = c("CD3+", "CD4+")

naive_CD4_tcell = c("CD3+", "CD4+","CCR7+","CD45RA+")
memory_CD4_tcell = c("CD3+", "CD4+","CCR7+","CD45RA-")
effector_CD4_tcell = c("CD3+", "CD4+","CCR7-","CD45RA+")
effector_memory_CD4_tcell = c("CD3+", "CD4+","CCR7-","CD45RA-")
naive_CD8_tcell = c("CD3+", "CD8+","CCR7+","CD45RA+")
memory_CD8_tcell = c("CD3+", "CD8+","CCR7+","CD45RA-")
effector_CD8_tcell = c("CD3+", "CD8+","CCR7-","CD45RA+")
effector_memory_CD8_tcell = c("CD3+", "CD8+","CCR7-","CD45RA-")

activated_naive_CD4_tcell = c("CD3+", "CD4+","CCR7+","CD45RA+", 
                              "CD38+", "HLA-DR+")
activated_memory_CD4_tcell = c("CD3+", "CD4+","CCR7+","CD45RA-",
                               "CD38+", "HLA-DR+")
activated_effector_CD4_tcell = c("CD3+", "CD4+","CCR7-","CD45RA+",
                                 "CD38+", "HLA-DR+")
activated_effector_memory_CD4_tcell = c("CD3+", "CD4+","CCR7-","CD45RA-",
                                        "CD38+", "HLA-DR+")
activated_naive_CD8_tcell = c("CD3+", "CD8+","CCR7+","CD45RA+",
                              "CD38+", "HLA-DR+")
activated_memory_CD8_tcell = c("CD3+", "CD8+","CCR7+","CD45RA-",
                               "CD38+", "HLA-DR+")
activated_effector_CD8_tcell = c("CD3+", "CD8+","CCR7-","CD45RA+",
                                 "CD38+", "HLA-DR+")
activated_effector_memory_CD8_tcell = c("CD3+", "CD8+","CCR7-","CD45RA-",
                                        "CD38+", "HLA-DR+")

t_helper_1_cell = c("CD3+", "CD4+","CCR7+","CD45RA+", "CXCR3+", "CCR6-")
t_helper_17_cell = c("CD3+", "CD4+","CCR7+","CD45RA+", "CXCR3-", "CCR6+")
t_helper2_cell = c("CD3+", "CD4+","CCR7+","CD45RA+", "CXCR3-", "CCR6-")

treg_cell = c("CD3+", "CD4+", "CCR4+", "CD25+", "CD127low")
memory_treg_cell = c("CD3+", "CD4+", "CCR4+", "CD25+", "CD127low", "CD45RO+")
naive_treg_cell = c("CD3+", "CD4+", "CCR4+", "CD25+", "CD127low", "CD45RO-")
activated_treg_cell = c("CD3+", "CD4+", "CCR4+", "CD25+", "CD127low", "HLA-DR+")

bcell = c("CD3-", "CD19+")
naive_bcell = c("CD3-", "CD19+", "CD27-")
memory_bcell = c("CD3-", "CD19+", "CD27+")
permablast = c("CD3-", "CD19+", "CD27+", "CD20-", "CD38+")
transitional_bcell = c("CD3-", "CD19+", "CD24high", "CD38high")

monocytes = c("CD3-", "CD19-", "CD14+")
CD16_monocyte = c("CD3-", "CD19-", "CD14+", "CD16+")
classical_monocyte = c("CD3-", "CD19-", "CD14+", "CD16-")
myeloid_dendritic_cell = c("CD3-", "CD19-", "CD14-", "CD20-",
                           "HLA-DR+", "CD11c+")
plasmacytoid_dentritic_cell =  c("CD3-", "CD19-", "CD14-", "CD20-",
                                 "HLA-DR+", "CD123+")
nk_cell =  c("CD3-", "CD19-", "CD14-", "CD20-", "CD16+")
