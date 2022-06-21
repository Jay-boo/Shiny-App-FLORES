# Treatment Part of the dataVizualisation project

 -  `import.R` : Import the different tables -> detect columns -> rename columns -> detect wich TC is referenced  -> write csv file with the according filename 



### Remarque `import.R`
    
**Rmq : La conversion dans l'etape avant de les écrires de sert a rien**

Il manque certaine table : TC5 , TC17 , TC25 et TC25 bis inexistant pour 2017

#### To do :

- [X] Add typology detection in the `main.R`
- [X] Code the write step 
- [X] Test with all the `TC` : `passed`
- [ ] Add `.xls` and `.xlsx` format
- [ ] Repérer l'année et creer un dossier spécifique pour chaque année dans la directory `output`

