package btcbalance;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

public class Gui {
	private static final String CLOJURE_CORE = "clojure.core";
	private static final String BTCBALANCE_CORE = "btcbalance.core";
	private static final String BTCBALANCE_ASSETS = "btcbalance.secure.assets";

	public static void main(final String[] args) {
		final IFn require = Clojure.var(CLOJURE_CORE, "require");
		
		
		require.invoke(Clojure.read(BTCBALANCE_ASSETS));
		final IFn app = Clojure.var(BTCBALANCE_ASSETS, "app");
		app.invoke();

	}
}
