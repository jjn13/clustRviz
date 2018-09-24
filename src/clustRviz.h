#include <RcppEigen.h>
#include <vector>
#include <set>
#include "clustRviz_logging.h"
#include <RProgress.h>

#define CLUSTRVIZ_CHECK_USER_INTERRUPT_RATE 50

// Helper to determine if STL set contains an element
//
// In general, this is not efficient because one wants to do something
// with the element and/or its location, but here we really only need containment
template <typename T>
bool contains(const std::set<T>& container, T element){
  typename std::set<T>::const_iterator it = container.find(element);
  return it != container.end();
}

// Prototypes - utils.cpp
void MatrixProx(const Eigen::MatrixXd&,
                Eigen::MatrixXd&,
                double,
                const Eigen::VectorXd&,
                bool);
