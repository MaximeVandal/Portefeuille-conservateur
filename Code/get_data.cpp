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
// Nom : get_data
//
// Description : Cette fonction permet de calculer tous les critères nécessaires pour former nos portefeuilles, ainsi que 
//               de récupérer d'autres données nécessaires ultérieurement.
//
// Arguments :
// - Top1000 (DataFrame) -> Tous nos titres disponibles lors des rééquilibrages, ainsi que leurs informations.
// - Mensuel (List)  -> Une liste dans laquelle toutes les informations sont regroupées par compagnie.
//
// Sortie :
// - volatility (NumericVector) -> Vecteur avec le calcul de volatilité de chaque titre pendant les trois dernières années.
// - NPY (NumericVector) -> Vecteur avec le calcul du NPY de chaque titre avec les données mensuelles des deux dernières années. 
// - momentum (NumericVector) -> Vecteur avec le calcul du momentum de chaque titre avec les données mensuelles de la dernière année. 
// - Dividendes (NumericVector) -> Vecteur du montant total de dividendes que chaque titre distribuera dans le prochain trimestre. 
// - Returns (NumericVector) -> Vecteur des rendements trimestriels de chaque titre.
//
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
List get_data(DataFrame Top1000, List Mensuel) {
  
  // Initialisation des variables
  int n = Top1000.nrows();
  
  NumericVector volatility(n);
  NumericVector dividend_yield(n);
  NumericVector buyback_yield(n);
  NumericVector NPY(n);
  NumericVector momentum(n);
  NumericVector Divid(n);
  NumericVector Next_PRCs(n);
  NumericVector Ret(n);
  
  CharacterVector comnam = Top1000["COMNAM"];
  CharacterVector date = Top1000["date"];
  NumericVector index = Top1000["Index"];
  NumericVector UnAn = Top1000["UnAn"];
  NumericVector DeuxAns = Top1000["DeuxAns"];
  NumericVector TroisAns = Top1000["TroisAns"];
  
  
  // Boucle sur tous les titres de Top1000
  for (int i = 0; i < n; i++) {
    
    String currentComnam = comnam[i];
    String currentDate = date[i];
    
    // Extraction des données du titre actuel
    List Comp = Mensuel[index[i]];
    
    CharacterVector date_mens = Comp["date"];
    NumericVector returns = Comp["RET"];
    NumericVector dividends = Comp["DIVAMT"];
    NumericVector prices = Comp["PRC"];
    NumericVector shares_out = Comp["SHROUT"];
    NumericVector marketcap = Comp["MarketCap"];
    NumericVector bid = Comp["BID"];
    
    
    // Trouver l'index du titre actuel dans les données mensuelles
    int it = distance(date_mens.begin(), find(date_mens.begin(), date_mens.end(), currentDate));
    
    // Création de vecteurs contenant les données pour la période nécessaire
    NumericVector vec_returns = NumericVector(returns.begin() + it - (int)TroisAns[i], returns.begin() + it);
    NumericVector vec_dividends = NumericVector(dividends.begin() + it - (int)UnAn[i], dividends.begin() + it);
    NumericVector vec_prices = NumericVector(prices.begin() + it - (int)DeuxAns[i], prices.begin() + it);
    NumericVector vec_marketcap = NumericVector(marketcap.begin() + it - (int)DeuxAns[i], marketcap.begin() + it);
    
    // Calcul de la somme des dividendes
    double sum_div = accumulate(vec_dividends.begin(), vec_dividends.end(), 0.0);
    
    
    // Nécessaire pour le calcul du rendement (Dividendes trimestriels et Rendements trimestriels)
    int min_it_3 = min(3, (int)date_mens.size() - it); //Cas où il manque des données
    
    // Dividendes trimestriels
    NumericVector vec_divid_trim = NumericVector(dividends.begin() + it, dividends.begin() + it + min_it_3); 
    double sum_div_Quarter = accumulate(vec_divid_trim.begin(), vec_divid_trim.end(), 0.0);
    
    // Rendements trimestriels
    NumericVector vec_ret_trim = NumericVector(returns.begin() + it, returns.begin() + it + min_it_3);
    double sum_ret_Quarter = 1;

    for (int i = 0; i < vec_ret_trim.size(); i++) { // Multiplie les rendements mensuels
      sum_ret_Quarter =  sum_ret_Quarter * (1 + vec_ret_trim[i]);
    }
    sum_ret_Quarter = sum_ret_Quarter - 1;
    
    
    // Calculer le nombre d'actions rachetées
    double sum_buyback = 0;
    for (int h = it - (int)DeuxAns[i]; h < it; h++) {
      double shrout = ((shares_out[h]-shares_out[h+1])*((prices[h] + prices[h+1])/2))/((marketcap[h]+marketcap[h+1])/2);
      sum_buyback += shrout;
    }
    
    // Calcul des métriques pour le titre i
    volatility[i] = sd(vec_returns);
    dividend_yield[i] = sum_div / prices[it];
    buyback_yield[i] = sum_buyback; 
    NPY[i] = dividend_yield[i] + buyback_yield[i];
    momentum[i] = (prices[it-1]/prices[it-UnAn[i]]) - 1;
    Divid [i] = sum_div_Quarter;
    Ret[i] = sum_ret_Quarter;
    
  }
  
  // Retourner les résultats
  return List::create(
    Named("volatility") = volatility,
    Named("NPY") = NPY,
    Named("momentum") = momentum,
    Named("Dividendes") = Divid,
    Named("Returns") = Ret
  );
}
