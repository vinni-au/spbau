package ru.spbau.storozhev.drunksim.objects;

public class BottleStuffObject implements IStuffObject {

	@Override
	public char toChar() {
		return 'B';
	}

	@Override
	public boolean isWalkableThru() {
		return false;
	}

}
