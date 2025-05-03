#include <Rcpp.h>
#include <iostream>
#include <cmath>
#include <vector>
#include <sstream>
#include <string>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace std;


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Nom : returns
//
// Description : Cette fonction permet de calculer les performances d'un portefeuille
//
// Arguments :
// - PF (List) -> Une liste regroupant toutes les informations des titres présents dans le portefeuille classées par trimestre.
// - dates (CharacterVector)  -> Vecteur des trimestres (dates).
// - investie (double)  -> Montant initial investi.
//
// Sortie :
// - date (CharacterVector) -> Vecteur des dates de chaque trimestre.
// - Rendement trimestre (NumericVector) -> Vecteur du rendement trimestriel du portefeuille à chaque trimestre.
// - Valeur (NumericVector) -> Vecteur de la valeur du portefeuille à chaque trimestre.
// - Turnover (NumericVector) -> Vecteur du nombre de transactions pour chaque trimestre.
//
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
List returns(List PF, CharacterVector dates, double investie) {
  
  // Initialisation des variables
  int n = dates.size();
  double EndValue = 0;
  double BegValue = investie;

  NumericVector returns(n);
  NumericVector turnover(n);
  NumericVector valeurs(n);
  
  valeurs[0] = investie;
  NumericVector Quantity;
  CharacterVector LastComnam;
  CharacterVector keep;
  CharacterVector sell;
  CharacterVector buy;
  
  // Boucle qui itère sur tous les trimestres
  for (int i = 0; i < n; i++) {
    
    // Extraction des données des titres
    List Now = PF[i];
    CharacterVector comnam = Now["COMNAM"];
    NumericVector prc = Now["PRC"];
    NumericVector bid = Now["BID"];
    NumericVector ask = Now["ASK"];
    NumericVector Nextdiv = Now["Dividendes"];
    NumericVector Returns = Now["Returns"];
    double montant_turnover = 0;
    
    int nb_titres = comnam.size();
    Quantity = (valeurs[i]/nb_titres)/ask;
    
    // Identification des titres à vendre
    if (i >= 1){
      int n_titres_last = distance(LastComnam.begin(),LastComnam.end());
      for (int z = 0; z < n_titres_last; z++) { // Itération sur les titres de la période précédente
        if (find(comnam.begin(), comnam.end(), LastComnam[z]) == comnam.end()) { // Si le titre de la période précédente n'est pas dans la période actuelle
          sell.push_back(comnam[z]);
        }
      }
    }
    
    
    // Itération sur les titres de la période actuelle
    double n_titres = distance(comnam.begin(),comnam.end());
    for (int j = 0; j < n_titres; j++) {
      
      // Identification des titres à garder
      if (find(LastComnam.begin(), LastComnam.end(), comnam[j]) != LastComnam.end()) { // Si le titre de la période actuelle est dans la période précédente
        keep.push_back(comnam[j]);
      }
      
      // Identification des titres à acheter
      else { // Si le titre de la période actuelle n'est pas dans la période précédente
        buy.push_back(comnam[j]);
        montant_turnover += Quantity[j]*ask[j];
      }
      
    }
    
    
    // Calcul du rendement du portefeuille
    BegValue = sum(ask * Quantity);
    EndValue = sum(Quantity * (bid * (1+Returns) + Nextdiv));
    returns[i+1] = (EndValue/BegValue) - 1;
    
    // Enregistrement des valeurs
    turnover[0] = 0;
    if (i >= 1) {
      turnover[i] = (montant_turnover / EndValue)*100;
    }

    valeurs[i+1] = valeurs[i]*(1 + returns[i+1]);
    
    // Mise à jour des valeurs pour le prochain trimestre
    LastComnam = comnam;
    
    
    // Nettoyage
    EndValue = 0 ;
    keep = CharacterVector();
    sell = CharacterVector();
    buy = CharacterVector();
  }
  
  // Retour des résultats
  return List::create(
    Named("date") = dates,
    Named("Rendement trimestre") = returns,
    Named("Valeur") = valeurs,
    Named("Turnover") = turnover
  );
}
