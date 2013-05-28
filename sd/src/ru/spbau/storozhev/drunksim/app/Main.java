package ru.spbau.storozhev.drunksim.app;

import ru.spbau.storozhev.drunksim.core.DrunkSimApp;

public class Main {

	public static void main(String[] args) {
		System.out.println("Starting DrunkSimApp...");
		System.out.println("-h or --help for usage");
		int w = 15;
		int h = 15;
		int steps = 200;
		boolean fullTrace = true;
		boolean hexagonal = false;
		
		for (int i = 0; i < args.length; ++i) {
			if (args[i] == "-f" || args[i] == "--full-trace") {
				fullTrace = true;
			}
			if (args[i] == "-x" || args[i] == "--hexagonal") {
				hexagonal = true;
			}
			if (args[i] == "-h" || args[i] == "--help") {
				System.out.println("Parameters: ");
				System.out.println("-f or --full-trace - print all steps");
				System.out.println("-x or --hexagonal - use hexagonal cells");
				System.out.println("-h or --help - print this help");
				return;
			}
		}
		
		DrunkSimApp app = new DrunkSimApp(w, h, hexagonal);
		try {
			if (fullTrace) {
				app.run(steps);
			} else {
				app.run(steps, new int[]{200, 300});
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		System.out.println("Done.");
	}
}
