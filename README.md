# rBed_bgmc

**Asymptotic Experiments with Data Structures: Bipartite Graph Matchings and Covers**

Quoted from our Abstract section:

> We consider instances of bipartite graphs and a number of asymptotic performance experiments in three projects: (1) top movie lists, (2) maximum matchings, and (3) minimum set covers. Experiments are designed to measure the asymptotic runtime performance of abstract data types (ADTs) in three programming languages: Java, R, and C++. The outcomes of these experiments may be surprising. In project (1), the best ADT in R consistently outperforms all ADTs in public domain Java libraries, including the library from Google. The largest movie list has 2^20 titles. In project (2), the Ford- Fulkerson algorithm implementation in R significantly outperforms Java. The hardest instance has 88452 rows and 729 columns. In project (3), a stochastic version of a greedy algorithm in R can significantly outperform a state-of-the-art stochastic solver in C++ on instances with num rows >= 300 and num columns >= 3000.

## Structure

* _data - contains all instances grouped in steiner3, orLib, and random.
* _data_RDS - contains all results generated from our local machine. Feel free to update RDS files once you replicate our experiments with your own results
* _data_tiny - contains all small instances.
* rBed_bgmc - main folder
  * bgmc_movieLib - contains all necessary codes and files for supporting the experiments in our first project: top movie lists
  * bgmc_matching - contains all necessary codes and files for supporting the experiments in our first project: maximum matchings
  * bgmc_covering - contains all necessary codes and files for supporting the experiments in our first project: minimum set covers

## Contact

If you have any question, feel free to reach me at lieason715@gmail.com

## Updates

The PDF version of our paper will be updated soon...
