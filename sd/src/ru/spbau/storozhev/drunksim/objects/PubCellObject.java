package ru.spbau.storozhev.drunksim.objects;

import ru.spbau.storozhev.drunksim.core.*;
import ru.spbau.storozhev.drunksim.stepdecisions.*;

public class PubCellObject extends AbstractCellObject {

	public PubCellObject(Cell c) {
		super(c);
	}

	@Override
	public boolean isWalkableThru() {
		return false;
	}

	@Override
	public char toChar() {
		return 'T';
	}

	@Override
	public AbstractStepDecision makeStep(int no) {
		if (no % 20 == 0) {
			return new GoOutStepDecision(cell.getX() + 1, cell.getY(), cell, new DrinkerCellObject(cell));
		}
		
		return null;
	}

}
