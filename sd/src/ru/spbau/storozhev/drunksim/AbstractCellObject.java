package ru.spbau.storozhev.drunksim;

public abstract class AbstractCellObject {
	public AbstractCellObject(Cell c) {
		cell = c;
	}
	
	public abstract boolean isWalkableThru();
	public abstract char toChar();
	public abstract AbstractStepDecision makeStep();
	
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