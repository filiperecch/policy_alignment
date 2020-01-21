
import requests
import xlrd
import pathlib
from pathlib import Path

project_folder = Path.cwd().parent

code_dict = {}
wb = xlrd.open_workbook(project_folder / "support_files" / "list_ibge_munic_code.xlsx")
sh = wb.sheet_by_index(0)

UF = ["AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS",
      "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"]

for i in range(5564):
    uf = sh.cell(i, 0).value
    code_munic = sh.cell(i, 1).value
    if uf in code_dict.keys():
        code_dict[uf].append(code_munic)
    else:
        code_dict[uf] = [code_munic]

#### Scraping PSE - states

for uf in UF:
    r = requests.post("http://simec.mec.gov.br/sase/sase_mapas.php?",
                      data = {"acao": "downloadEstado",
                              "estuf": uf})
    pse_state = project_folder / "pme_pse_doc_analysis_cloud_dir/data/raw/pse"
    state_file = uf + ".pdf"
    with open(pse_state/state_file, "wb") as f:
        f.write(r.content)

#### Scraping PME - municipalities

for uf in UF:
    for munic in code_dict[uf]:
        r = requests.post("http://simec.mec.gov.br/sase/sase_mapas.php?",
                          data = {"acao": "download",
                                  "estuf": uf,
                                  "muncod": [int(munic), int(munic)]})
        munic_file = uf+str(int(munic))+".pdf"
        pme_state = project_folder/"pme_pse_doc_analysis_cloud_dir/data/raw/pme"/uf
        pathlib.Path(pme_state).mkdir(parents=True, exist_ok=True)
        with open(pme_state/munic_file, "wb") as f:
            f.write(r.content)



