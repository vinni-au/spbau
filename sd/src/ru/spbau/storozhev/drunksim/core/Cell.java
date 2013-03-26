package ru.spbau.storozhev.drunksim.core;

import ru.spbau.storozhev.drunksim.objects.*;

public class Cell {
	public Cell(Field f, int i, int j, AbstractCellObject o) {
		field = f;
		x = i;
		y = j;
		object = o;
	}
	
	public int getX() {
		return x;
	}
	
	public int getY() {
		return y;
	}
	
	public Field getField() {
		return field;
	}
	
	public AbstractCellObject getObject() {
		return object;
	}
	
	public void setCellObject(AbstractCellObject o) {
		object = o;
	}
	
	private int x;
	private int y;
	private AbstractCellObject object;
	private Field field;
}
