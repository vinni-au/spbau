package ru.spbau.storozhev.patterns.lights;

public interface Illuminant extends Iterable<LightBulb> {
	void turnOn() throws BurnedOutException;
	void turnOff();
	boolean isOn();
	boolean isOk();
	void visit(Visitor visitor);
}