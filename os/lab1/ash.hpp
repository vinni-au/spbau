/**************************
 * Operating systems
 * Lab. No 1. Command Shell
 * Ash - Anton's shell
 *
 * (c) Anton Storozhev, 2012
 * antonstorozhev@gmail.com
 ***************************/
 #ifndef _ASH_HPP_
 #define _ASH_HPP_
 
//! Ash command shell 
class Ash
{
public:
	explicit Ash();
	
	void run(bool showinfo = true);
	
private:
	void showHelp();
	
};

#endif