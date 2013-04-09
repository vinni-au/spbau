package ru.spbau.storozhev.drunksim.objects;

import ru.spbau.storozhev.drunksim.core.*;
import ru.spbau.storozhev.drunksim.stepdecisions.*;

public abstract class AbstractCellObject {
	public AbstractCellObject(Cell c) {
		cell = c;
	}
	
	public abstract boolean isWalkableThru();
	public abstract char toChar();
	public abstract AbstractStepDecision makeStep(int no);
	
	public int getX() {
		return cell.getX();
	}
	
	public int getY() {
		return cell.getY();
	}
	
	public Cell getCell() {
		return cell;
	}
	
	public void setCell(Cell c) {
		cell = c;
	}
	
	protected Cell cell;
}
