import sys
import subprocess

if len(sys.argv) == 2:
    Instance = sys.argv[1]
    BaseAlgs = ["MOD", "IM", "LC", "OSLOM"]
    Frameworks = ["RemoveEdge", "ReduceEdge", "ReduceWeight"]
    LayersNum = ["2", "3", "4", "5"]

    for BaseAlg in BaseAlgs:
        print("--$BaseAlg")
        subprocess.call(f"mkdir -p {Instance}/{BaseAlg}", shell=True)
        for Framework in Frameworks:
            print("----$Framework")
            subprocess.call(f"mkdir {Instance}/{BaseAlg}/{Framework}", shell=True)
            for Num in LayersNum:
                FileName=f"{Instance}/{BaseAlg}/{Framework}/{Num}.config"
                print(FileName)
                subprocess.call(f"cp {Instance}.config {FileName}", shell=True)
                Source = "MOD\/++MAX\/2"
                Direct = f"{BaseAlg}\/{Framework}\/{Num}"
                if Framework == "RemoveEdge":
                    FMethod="Remove"
                    WGraph="FALSE"

                if Framework == "ReduceEdge":
                    FMethod="Reduce++"
                    WGraph="FALSE"

                if Framework == "ReduceWeight":
                    FMethod="Reduce++"
                    WGraph="TRUE"

                if BaseAlg == "LC":
                    SingleMethod="LinkCommunity"

                if BaseAlg == "MOD":
                    SingleMethod="Modularity"
                
                if BaseAlg == "IM":
                    SingleMethod="Infomap"
                
                if BaseAlg == "OSLOM":
                    SingleMethod="OSLOM"
                
                subprocess.call(f"sed -i 's/{Source}/{Direct}/g' {FileName}", shell=True)
                subprocess.call(f"sed -i 's/SingleLayer_Method\tModularity/SingleLayer_Method\t{SingleMethod}/g' {FileName}", shell=True)
                subprocess.call(f"sed -i 's/Number_Of_Layers\t2/Number_Of_Layers\t{Num}/g' {FileName}", shell=True)
                subprocess.call(f"sed -i 's/Frameworks\tReduce++/Frameworks\t{FMethod}/g' {FileName}", shell=True)
                subprocess.call(f"sed -i 's/TRUE/{WGraph}/g' {FileName}", shell=True)
else:
    print(f"Usage: {sys.argv[0]} UniversityNumber")
    print(f"e.g.: {sys.argv[0]} Caltech36")