#include <iostream>
#include <typeinfo>
#include <string>

class space_body;
class asteroid;
class space_shuttle;

class collider {
  public:
    collider(space_body& ) : 
      name("space body") 
      {  }

    collider(asteroid& ) :
      name("asteroid") 
      {  }

    collider(space_shuttle& ) :
      name("space shuttle")
      {  }

    void collide() {
      std::cout << "I collided with " << name << std::endl;
    }
  private:
    std::string name;
};

class space_body {
 public:
  virtual void collide(space_body&) = 0;
};

class asteroid : public space_body {
 public:
  void collide(space_body& other) {
    collider(*this).collide();
  }
};

class space_shuttle : public space_body {
 public:
  void collide(space_body& other) {
    collider(*this).collide();
  }
};


int main(int ARGC, char *ARGV[]) {
  asteroid vesta, ceres;
  space_shuttle endeavour, discovery;
  
  // Let's make some science!
  vesta.collide(ceres); // Boom!
  vesta.collide(endeavour); // Boom!
  discovery.collide(endeavour); // Oops!

  return 0;
}
