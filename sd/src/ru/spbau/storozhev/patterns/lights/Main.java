package ru.spbau.storozhev.patterns.lights;

import java.util.List;
import java.util.ArrayList;

public class Main {

	public static void main(String[] args) {
		/*
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
		*/
		List<LightBulb> cb = new ArrayList<>();
		cb.add(new LightBulb());
		cb.add(new LightBulb());
		cb.add(new LightBulb());
		List<LightBulb> gb = new ArrayList<>();
		gb.add(new LightBulb());
		gb.add(new LightBulb());
		Candelier c = new Candelier(cb);
		Garland g = new Garland(gb);
		List<Illuminant> lsl = new ArrayList<>();
		lsl.add(c);
		lsl.add(new LightBulb());
		lsl.add(g);
		CompositeLightingSystem ls = new CompositeLightingSystem(lsl);
		List<Illuminant> lsl2 = new ArrayList<>();
		lsl2.add(ls);
		lsl2.add(new LightBulb());
		CompositeLightingSystem ls2 = new CompositeLightingSystem(lsl2);

		System.out.println("--------- Checking lightbulb iterator ----------");
		for (LightBulb bulb : new LightBulb()) {
			System.out.println("Found lightbulb: " + bulb.toString());
		}
		System.out.println();
		
		System.out.println("--------- Checking candelier iterator ----------");
		for (LightBulb bulb : c) {
			System.out.println("Found lightbulb: " + bulb.toString());
		}
		System.out.println();
		
		System.out.println("--------- Checking garland iterator ----------");
		for (LightBulb bulb : g) {
			System.out.println("Found lightbulb: " + bulb.toString());
		}
		System.out.println();
		
		System.out.println("--------- Checking composite LS iterator -----------");
		for (LightBulb bulb : ls2) {
			System.out.println("Found lightbulb: " + bulb.toString());
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
