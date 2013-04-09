package ru.spbau.storozhev.drunksim.objects;

import ru.spbau.storozhev.drunksim.core.*;
import ru.spbau.storozhev.drunksim.stepdecisions.*;

public class DummyCellObject extends AbstractCellObject {
	public DummyCellObject(Cell c) {
		super(c);
	}

	@Override
	public boolean isWalkableThru() {
		return false;
	}

	@Override
	public char toChar() {
		return ' ';
	}

	@Override
	public AbstractStepDecision makeStep(int no) {
		return null;
	}

}
