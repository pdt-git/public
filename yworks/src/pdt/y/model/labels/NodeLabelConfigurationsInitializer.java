package pdt.y.model.labels;

import java.util.Map;

import y.view.NodeLabel;
import y.view.YLabel;

public class NodeLabelConfigurationsInitializer {
	public static void initialize() {
		registerLabel(new PrefixLabel());
		registerLabel(new PostfixLabel());
		registerLabel(new BracketLabel());
		registerLabel(new MiddleLabel());
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	private static void registerLabel(CroppingLabelBase label) {
		YLabel.Factory factory = NodeLabel.getFactory();

		Map implementationsMap = factory.createDefaultConfigurationMap();
		
		implementationsMap.put(YLabel.Painter.class, label);
		implementationsMap.put(YLabel.Layout.class, new CroppingLabelLayout());

		// Add the first configuration to the factory.
		factory.addConfiguration(label.getClass().getSimpleName(), implementationsMap);
	}
}
