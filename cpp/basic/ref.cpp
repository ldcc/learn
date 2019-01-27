#include <string>
#include <iostream>

using namespace std;

int main() {

  string foo = "I am foo";
  string& fooRef = foo;
  
  fooRef += ". Hi!\n";
  std::cout << fooRef << foo;

}
