# Treatment Part of the dataVizualisation project

 -  `import.R` : Import the different tables -> detect columns -> rename columns -> detect wich TC is referenced  -> write csv file with the according filename 


<hr/>
<br/>

## TO DO 

- **1. Reecrire les fonction d'import de manières à rendre un bilan** DONE
- **2. Réecrire update**
- **3.Faire le test de import files avec TC6 et TC7 de 2018**

<br/>

<hr/>

## Structure data inputs :

- inputs
    - `report_imports.csv`
    - 2017
        - `infra_dep`
            - EPCI
        - `reg_dep`
            - TC1
            - TC2
            - ...
        - `cmd_files`

    - 2018
        - `infra_dep`
            - EPCI
        - `reg_dep`
            - TC1
            - TC2
            - ...
        - `cmd_files`
    - 2019
        - ...

<hr/>


## Structure data outputs :

- outputs
    - 2017
        - Nomenclatures
        - TC1 ... + EPCI(s)

    - 2018
        - Nomenclatures
        - TC1 ... + EPCI(s)

    - 2019
        - ...

<hr/>


### Remarque `import.R`
    
**Rmq : La conversion dans l'etape avant de les écrires de sert a rien**

Il manque certaine table : TC5 , TC17 , TC25 et TC25 bis inexistant pour 2017

#### To do :

- [X] Add typology detection in the `main.R`
- [X] Code the write step 
- [X] Test with all the `TC` : `passed`
- [ ] Add `.xls` and `.xlsx` format
- [ ] Repérer l'année et creer un dossier spécifique pour chaque année dans la directory `output`

### Remarque `update.R`

Le probleme de detection des differences est que les nom inputs/outputs ne correspondent pas forcement
Ainsi une solution pourrait etre de simplement tenir un report d'importation

= > Dans import.R a chaque fichier importé il ecrit le nom du fichier importé dans un report .Rdata
Remarque : Ici ce n'est pas grave de le stocker en local , il n'a pas vocation à etre modifié lors de la mise ne ligne de l'appli.
Structure du report: 
directory(year) |scale | filename

### EPCI problem

https://www.insee.fr/fr/information/2510634
