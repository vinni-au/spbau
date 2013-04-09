package ru.spbau.storozhev.drunksim.core;

import ru.spbau.storozhev.drunksim.objects.*;

public class Field {
	Field(int w, int h) {
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
	
	public void setCellObject(AbstractCellObject o) {
		int x = o.getX();
		int y = o.getY();
		
		if (x < 0 || x >= width || y < 0 || y >= height)
			return;
		
		cells[x][y].setCellObject(o);
	}
	
	public void setStuffObject(IStuffObject o, int x, int y) {
		if (x < 0 || x >= width || y < 0 || y >= height)
			return;
		
		cells[x][y].setStuffObject(o);
	}
	
	public AbstractCellObject getCellObject(int x, int y) {
		if (x < 0 || x >= width || y < 0 || y >= height)
			return null;
		
		return cells[x][y].getObject();
	}
	
	public IStuffObject getStuffObject(int x, int y) {
		if (x < 0 || x >= width || y < 0 || y >= height)
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
		return cells[x][y];
	}
	
	public void print() {
		for (int i = 0; i < width; ++i) {
			for (int j = 0; j < height; ++j) {
				if (cells[i][j].getStuff() == null) {
					System.out.print(cells[i][j].getObject().toChar());
				} else {
					System.out.print(cells[i][j].getStuff().toChar());
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
	
	private int width;
	private int height;
	private Cell[][] cells;
}
