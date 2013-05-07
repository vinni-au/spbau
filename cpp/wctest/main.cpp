#include <iostream>
#include <set>
#include <algorithm>
#include <locale>

using namespace std;

int main() {
  ios_base::sync_with_stdio(false);
  std::set<char> letters;
  wchar_t letter = (wchar_t)u'а';
  wstring str(L"привет");
  str.push_back(letter);
  wcout << str << endl;
  wcout << L"привет hi to all" << endl;
  return 0;
}
