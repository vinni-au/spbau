package ru.spbau.storozhev.patterns.lights;

import java.util.List;
import java.util.ArrayList;

public class Main {

	public static void main(String[] args) {
		List<Illuminant> samples = new ArrayList<>();
		samples.add(new LightBulb());
		samples.add(createCandelier());
		samples.add(createGarland());
		samples.add(createLightingSystem());
		
		
		for (Illuminant illuminant : samples) {
			System.out.println("-------------------------------");
			for (int i = 0; i < 10; ++i) {
				try {
					illuminant.turnOn();
				} catch (BurnedOutException e) {
					System.out.println("Test illuminant has burned out");
				}
				illuminant.turnOff();
			}
		}
	}
	
	private static Candelier createCandelier() {
		List<LightBulb> list = new ArrayList<>();
		for (int i = 0; i < 3; ++i) {
			list.add(new LightBulb());
		}
		return new Candelier(list);
	}
	
	private static Garland createGarland() {
		List<LightBulb> list = new ArrayList<>();
		for (int i = 0; i < 5; ++i) {
			list.add(new LightBulb());
		}
		return new Garland(list);
	}
	
	private static CompositeLightingSystem createLightingSystem() {
		List<Illuminant> list = new ArrayList<>();
		list.add(createGarland());
		list.add(new LightBulb());
		list.add(createGarland());
		list.add(createCandelier());
		list.add(new LightBulb());
		return new CompositeLightingSystem(list);
	}

}
