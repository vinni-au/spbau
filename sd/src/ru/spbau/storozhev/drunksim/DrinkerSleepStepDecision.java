package ru.spbau.storozhev.drunksim;

public class DrinkerSleepStepDecision extends AbstractStepDecision {

	public DrinkerSleepStepDecision(int x, int y, Cell c) {
		super(x, y, c);
	}

	@Override
	public void doIt() {
		DrinkerCellObject drinker = (DrinkerCellObject)cell.getObject();
		drinker.setSleepSteps(5);
		System.out.println("Going to sleep...");
//		throw new RuntimeException();
	}

	@Override
	public boolean isConflictedWith(AbstractStepDecision other) {
		return true;
	}

}
