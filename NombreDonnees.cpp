#include <Rcpp.h>
#include <iostream>
#include <cmath>
#include <vector>
#include <sstream>
#include <string>
#include <algorithm> 

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace std;


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Nom : NombreDonnees
//
// Description : Cette fonction permet de trouver le nombre de données mensuelles disponibles 1,2 et 3 ans dans le passé 
//               pour chaque titre de Top1000.
//
// Arguments :
// - Mensuel (List)  -> Une liste dans laquelle toutes les informations sont regroupées par compagnie.
// - nomTop1000 (CharacterVector) -> Tous les noms des compagnies de Top1000.
// - DateTop1000 (DateVector) -> Toutes les dates de Top1000.
// - MoinsUn (DateVector) -> Chaque date du Top1000 soustraite d'un an.
// - MoinsDeux (DateVector) -> Chaque date du Top1000 soustraite de deux ans.
// - MoinsTrois (DateVector) -> Chaque date du Top1000 soustraite de trois ans.
//
// Sortie :
// - Compte_1 (NumericVector) -> Vecteur du nombre de données mensuelles de chaque titre dans la dernière année. 
// - Compte_1 (NumericVector) -> Vecteur du nombre de données mensuelles de chaque titre dans les deux dernières années. 
// - Compte_1 (NumericVector) -> Vecteur du nombre de données mensuelles de chaque titre dans les trois dernières années. 
//
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
List NombreDonnees(List Mensuel, CharacterVector nomTop1000, DateVector DateTop1000, DateVector MoinsUn, DateVector MoinsDeux, DateVector MoinsTrois) {
  int n = nomTop1000.size();
  NumericVector compte_1(n);
  NumericVector compte_2(n);
  NumericVector compte_3(n);
  
  for (int i = 0; i < n; ++i) {
    
    // Trouver le nom et la date de la compagnie visée
    String CurrentName = nomTop1000[i];
    Date Currentdate = DateTop1000[i];
    
    // Ouvrir la liste de la compagnie dans Mensuel 
    List Comp = Mensuel[CurrentName];
    DateVector dates = Comp["date"];
    

    // Trouver le nombre de données pour la dernière année
    LogicalVector condition_1 = (dates >= MoinsUn[i]) & (dates <= Currentdate + 10);
    LogicalVector condition_2 = (dates >= MoinsDeux[i]) & (dates <= Currentdate + 10);
    LogicalVector condition_3 = (dates >= MoinsTrois[i]) & (dates <= Currentdate + 10);
    
    // Enregistrement du nombre de données
    compte_1[i] = sum(condition_1);
    compte_2[i] = sum(condition_2);
    compte_3[i] = sum(condition_3);

  }
  
  return List::create(
    Named("Compte_1") = compte_1,
    Named("Compte_2") = compte_2,
    Named("Compte_3") = compte_3);
}
