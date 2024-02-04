#include "polynomial.h"
#include <algorithm>
// #include <limits>
// #include <unordered_map>
#include <string>
#include <sstream>

using namespace std;



void Polynomial::normalize ()
{
	//orig

	if (degree < 0)
	{
		terms.clear();
		return;
	}

	//std::list<Term>::iterator it = terms.begin();
	terms.sort([](const Term &b,const Term &a){return a.power >b.power;});
	auto it = terms.begin();
	while (it != terms.end())
	{
		auto current = it;
		++it;
		while(it != terms.end() && current->power==it ->power){
			current->coefficient += it->coefficient;
			it = terms.erase(it);

		}
	}
	terms.remove_if([](const Term &term){return term.coefficient == 0;});
	if (terms.begin() != terms.end()) {
		degree = terms.back().power;
	}
	else{
		degree = 0;
	}
	//end orig



}




Polynomial::Polynomial ()
: degree(-1),terms()
{
}

Polynomial::Polynomial (int b, int a)
: degree(1),terms()
{
	//coefficients[0] = b;
	//coefficients[1] = a;
	terms.push_back(Term(b,0));
	terms.push_back(Term(a,1));
	normalize();
}

// Polynomial::Polynomial(Term term)
// : degree(term.power)
// {
// 	terms.push_back(term);
// 	normalize();
// }

Polynomial::Polynomial (std::initializer_list<Term> init)
: degree(1),terms()//,terms(term.begin(),term.end())
{
	// for(const Term &term : terms){
	// 	this->terms.push_back(Term(term.coefficient,term.power));
	// // }
	for (const auto &term : init)
    {
        terms.push_back(term);
    }
	normalize();

}


Polynomial::Polynomial (int nC, int coeff[])
: degree(nC-1),terms()
{
	for (int i = 0; i <= degree; i++){
		terms.push_back(Term(coeff[i],i));
	}
	normalize();
}



// void Polynomial::normalize ()
// {
// 	// while (degree+1 > 1 && coefficients[degree] == 0)
// 	// 	--degree;
// 	// for (auto it = terms.begin(); it != terms.end(); ++it){
// 	// 	if(it->coefficient != 0){
// 	// 		degree = it->power;
// 	// 	}
// 	// }
// 	cout<< "some";

// }


int Polynomial::getDegree() const
{
	return degree;
}

int Polynomial::getCoeff(int power) const
{
	// // if (power >= 0 && power <= degree)
	// // {
		for (auto it = terms.begin(); it != terms.end(); ++it){
			if(it->power == power)
				return it->coefficient;

		}

		return 0;
}

Polynomial Polynomial::operator+ (const Polynomial& p) const
{
	if (degree == -1 || p.degree == -1)
		return Polynomial();

	int resultSize = max(degree+1, p.degree+1);
	int *resultCoefficients = new int[resultSize];
	int k = 0;

	auto it = terms.begin();
	auto it2 = p.terms.begin();
	while (k <= getDegree() && k <= p.getDegree())
	{
		//resultCoefficients[k] = coefficients[k] + p.coefficients[k];

		resultCoefficients[k] = this->getCoeff(k) + p.getCoeff(k);
		++k;
	}

	for (int i = k; i <= getDegree(); ++i)
	    resultCoefficients[i] = this->getCoeff(i);

	for (int i = k; i <= p.getDegree(); ++i)
	    resultCoefficients[i] = p.getCoeff(i);

	Polynomial result(resultSize, resultCoefficients);
	delete[] resultCoefficients;
	result.normalize();
	return result;



}


Polynomial Polynomial::operator* (int scale) const
{


	if (degree == -1)
        return Polynomial();

    Polynomial result (*this);
    for (auto &term : result.terms)
    {
        term.coefficient *= scale;
    }

    result.normalize();
    return result;
}

Polynomial Polynomial::operator* (Term term) const
{
	if (degree == -1){
		return Polynomial();
	}
	int *results = new int[degree + 1 + term.power];

	for (int i = 0; i < term.power; ++i){
		results[i] = 0;
}
	//int i= 0;
	for (int i = 0; i< degree +1; ++i){
		 results[i+term.power] = getCoeff(i) * term.coefficient;
		// ++i;

	}

	Polynomial result (degree + 1 + term.power, results);
	delete [] results;
	result.normalize();
	return result;

}


void Polynomial::operator*= (int scale)
{
	// if (degree == -1)
	// 	return;
	// for (auto it = terms.begin(); it != terms.end(); ++it)
	// 	//coefficients[i] = scale * coefficients[i];
	// 	it->coefficient = scale * it->coefficient;
	// normalize();

	for (auto &term : terms){
		term.coefficient *= scale;
	}
	normalize();
}


Polynomial Polynomial::operator/ (const Polynomial& denominator) const
{
	if (degree == -1 || denominator.degree == -1)
		return Polynomial();
	if (*this == Polynomial(0))
		return *this;
	if (denominator.getDegree() > getDegree())
		return Polynomial();


	int resultSize = degree - denominator.degree + 1;
	int* results = new int[resultSize];
	for (int i = 0; i < resultSize; ++i)
		results[i] = 0;

	Polynomial remainder = *this;
	for (int i = resultSize-1; i >= 0; --i)
	{
		// Try to divide remainder by denominator*x^(i-1)
		int remainder1stCoeff = remainder.getCoeff(i+denominator.getDegree());
		int denominator1stCoeff = denominator.getCoeff(denominator.getDegree());


		if (remainder1stCoeff % denominator1stCoeff == 0) {
			results[i] = remainder1stCoeff / denominator1stCoeff;
			Polynomial subtractor = denominator * Term(-results[i], i);
			remainder = remainder + subtractor;
		} else {
			// Can't divide this
			break;
		}
	}
	if (remainder == Polynomial(0)) {
		Polynomial result (resultSize, results);
		delete [] results;
		return result;
	}
	else
	{
		// A non-zero remainder could not be removed - division fails
		delete [] results;
		return Polynomial();
	}

}


bool Polynomial::operator==(const Polynomial& p) const{

	if (degree != p.degree)
        return false;

    auto it1 = terms.begin();
    auto it2 = p.terms.begin();

	for(; it2 != p.terms.end();++it1){
		if(it1 == terms.end()|| it1->coefficient != it2->coefficient){
			return false;
		}
		++it1;
	}

	return it1 ==terms.end() && it2 == p.terms.end();
	return true;
}


