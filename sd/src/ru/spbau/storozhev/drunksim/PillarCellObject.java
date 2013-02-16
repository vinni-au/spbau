package ru.spbau.storozhev.drunksim;

public class PillarCellObject extends AbstractCellObject {

	public PillarCellObject(Cell c) {
		super(c);
	}

	@Override
	public boolean isWalkableThru() {
		return false;
	}

	@Override
	public char toChar() {
		return 'C';
	}

	@Override
	public AbstractStepDecision makeStep() {
		return null;
	}

}
