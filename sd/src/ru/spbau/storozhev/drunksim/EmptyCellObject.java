package ru.spbau.storozhev.drunksim;

public class EmptyCellObject extends AbstractCellObject {

	public EmptyCellObject(Cell c) {
		super(c);
	}

	@Override
	public boolean isWalkableThru() {
		return true;
	}

	@Override
	public char toChar() {
		return '.';
	}

	@Override
	public AbstractStepDecision makeStep() {
		return null;
	}

}
