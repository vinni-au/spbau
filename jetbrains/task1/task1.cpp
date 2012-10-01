#include <iostream>
#include <list>

struct Tree
{
	int data;
	Tree* left;
	Tree* right;

	Tree(int d = 0) :
		data(d),
		left(0), right(0)
	{	}

	void setLeft(Tree* node)
	{
		left = node;
	}

	void setRight(Tree* node)
	{
		right = node;
	}

};

void print(Tree* root)
{
	if (root) {
		if (root->left)
			print(root->left);
		std::cout << root->data << " ";
		if (root->right)
			print(root->right);
	}
}

bool checkIfBST(Tree* tree)
{
	static int value = 0;
	static bool check = false;
	if (tree) {
		if (tree->left)
			if (!checkIfBST(tree->left))
				return false;
		if (check) {
			if (tree->data < value)
				return false;
			else {
				value = tree->data;
			}
		} else {
			check = true;
			value = tree->data;
		}
		if (tree->right)
			if (!checkIfBST(tree->right))
				return false;			
	}
	return true;
}

int values[] = {    8, 
            4,               12,
        2,        6,      10,        14,
	1,     3,  5,    7, 9,   11,  13,   15};

Tree* makeTree()
{
	Tree* root = new Tree(values[0]);
	std::list<Tree*> list;
	list.push_back(root);
	int i = 1;
	while (i < sizeof(values)/sizeof(int)) {
		Tree* current = *list.begin();
		list.pop_front();
		current->setLeft(new Tree(values[i++]));
		current->setRight(new Tree(values[i++]));
		list.push_back(current->left);
		list.push_back(current->right);
	}
	return root;
}

int main()
{
	Tree* tree = makeTree();
	print(tree);
	std::cout << std::endl;
	std::cout << (checkIfBST(tree) ? "YES" : "NO")  << std::endl;
	return 0;
}
