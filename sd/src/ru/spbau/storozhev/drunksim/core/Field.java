package ru.spbau.storozhev.drunksim.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import ru.spbau.storozhev.drunksim.objects.*;

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
		
		cells[x][y].setStuffObject(o);
	}
	
	public AbstractCellObject getCellObject(int x, int y) {
		if (!checkBounds(x, y))
			return null;
		
		return cells[x][y].getObject();
	}
	
	public IStuffObject getStuffObject(int x, int y) {
		if (!checkBounds(x, y))
			return null;
		
		return cells[x][y].getStuffObject();
	}
	
	public int getWidth() {
		return width;
	}
	
	public int getHeight() {
		return height;
	}
	
	public Cell getCell(int x, int y) {
		return cells[x][y];
	}
	
	public List<Cell> getNeighbours(int x, int y) {
		List<Cell> result = new ArrayList<>();
		if (hexagonal) {
			
		} else {
			
		}
		return Collections.unmodifiableList(result);
	}
	
	public void print() {
		for (int i = 0; i < width; ++i) {
			for (int j = 0; j < height; ++j) {
				if (cells[i][j].getStuffObject() == null) {
					System.out.print(cells[i][j].getObject().toChar());
				} else {
					System.out.print(cells[i][j].getStuffObject().toChar());
				}
			}
			System.out.println();
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
