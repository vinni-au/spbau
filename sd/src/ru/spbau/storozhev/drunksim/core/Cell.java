package ru.spbau.storozhev.drunksim.core;

import ru.spbau.storozhev.drunksim.objects.AbstractCellObject;
import ru.spbau.storozhev.drunksim.objects.IStuffObject;


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
	
	public void setStuff(IStuffObject o) {
		stuff = o;
	}
	
	public IStuffObject getStuff() {
		return stuff;
	}
	
	public boolean isWalkableThru() {
		boolean result = false;
		if (stuff != null) {
			result = stuff.isWalkableThru();
			return result && object.isWalkableThru();
		} else {
			return object.isWalkableThru();
		}
	}
	
	private int x;
	private int y;
	private AbstractCellObject object;
	private IStuffObject stuff;
	private Field field;
}
