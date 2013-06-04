package ru.spbau.storozhev.patterns.lights;

public abstract class AbstractIlluminant implements Iterable<LightBulb>, Illuminant  {

	public abstract void turnOn() throws BurnedOutException;	
	public abstract void turnOff();

	@Override
	public boolean isOn() {
		return isOn;
	}
	
	@Override
	public boolean isOk() {
		return isOk;
	}
	
	protected boolean isOn = false;
	protected boolean isOk = true;
}
