package ru.spbau.storozhev.drunksim.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import ru.spbau.storozhev.drunksim.objects.AbstractCellObject;
import ru.spbau.storozhev.drunksim.objects.EmptyCellObject;
import ru.spbau.storozhev.drunksim.objects.IStuffObject;

public class Field {
	public Field(int w, int h) {
		width = w;
		height = h;
		cells = new Cell[w][h];
		for (int i = 0; i < w; ++i) {
			for (int j = 0; j < h; ++j) {
				cells[i][j] = new Cell(this, i, j, null);
				cells[i][j].setCellObject(new EmptyCellObject(cells[i][j]));
			}
		}
	}
	
	public Field(int w, int h, boolean hexagonal) {
		this(w, h);
		this.hexagonal = hexagonal;
	}
	
	public void setCellObject(AbstractCellObject o) {
		int x = o.getX();
		int y = o.getY();
		
		if (!checkBounds(x, y))
			return;
		
		cells[x][y].setCellObject(o);
	}
	
	public void setStuffObject(IStuffObject o, int x, int y) {
		if (!checkBounds(x, y))
			return;
		
		cells[x][y].setStuff(o);
	}
	
	public AbstractCellObject getCellObject(int x, int y) {
		if (!checkBounds(x, y))
			return null;
		
		return cells[x][y].getObject();
	}
	
	public IStuffObject getStuffObject(int x, int y) {
		if (!checkBounds(x, y))
			return null;
		
		return cells[x][y].getStuff();
	}
	
	public int getWidth() {
		return width;
	}
	
	public int getHeight() {
		return height;
	}
	
	public Cell getCell(int x, int y) {
		if (!checkBounds(x, y))
			return null;
		
		return cells[x][y];
	}
	
	public List<Cell> getNeighbours(int x, int y) {
		List<Cell> result = new ArrayList<>();
		if (hexagonal) {
			if (x % 2 == 0) {
				if (checkBounds(x - 1, y - 1))
					result.add(getCell(x - 1, y - 1));
				if (checkBounds(x + 1, y - 1))
					result.add(getCell(x + 1, y - 1));
			} else {
				if (checkBounds(x - 1, y + 1))
					result.add(getCell(x - 1, y + 1));
				if (checkBounds(x + 1, y + 1))
					result.add(getCell(x + 1, y + 1));
			}			
		}
		
		if (checkBounds(x + 1, y))
			result.add(getCell(x + 1, y));
		if (checkBounds(x, y - 1))
			result.add(getCell(x, y - 1));
		if (checkBounds(x - 1, y))
			result.add(getCell(x - 1, y));
		if (checkBounds(x, y + 1))
			result.add(getCell(x, y + 1));

		return Collections.unmodifiableList(result);
	}
	
	public List<Cell> getNeighbours(Cell cell) {
		return getNeighbours(cell.getX(), cell.getY());
	}
	
	public void print() {
		for (int i = 0; i < width; ++i) {
			if (hexagonal) {
				if (i%2 == 1) {
					System.out.print(' ');
				}
			}

			for (int j = 0; j < height; ++j) {
				printCell(i, j);
				if (hexagonal) {
					System.out.print(' ');
				}
			}
			System.out.println();
		}
	}
	
	private void printCell(int i, int j) {
		if (cells[i][j].getStuff() == null) {
			System.out.print(cells[i][j].getObject().toChar());
		} else {
			System.out.print(cells[i][j].getStuff().toChar());
		}
	}
	
	public void print(int step, String info) {
		System.out.println("Step " + step);
		if (info.length() > 0)
			System.out.println(info);
		print();
	}
	
	public void print(int step) {
		print(step, "");
	}
	
	private boolean checkBounds(int x, int y) {
		if (x < 0 || x >= width || y < 0 || y >= height)
			return false;
		
		return true;
	}
	
	private int width;
	private int height;
	private Cell[][] cells;
	private boolean hexagonal = false;
}
