package ru.spbau.storozhev.drunksim.stepdecisions;

import ru.spbau.storozhev.drunksim.core.*;

public abstract class AbstractStepDecision {
	public AbstractStepDecision(int x, int y, Cell c) {
		targetX = x;
		targetY = y;
		cell = c;
	}
	
	public abstract void doIt();
	
	public int getTargetX() {
		return targetX;
	}
	
	public int getTargetY() {
		return targetY;
	}
	
	public Cell getCell() {
		return cell;
	}
	
	protected int targetX;
	protected int targetY;
	protected Cell cell;
}
