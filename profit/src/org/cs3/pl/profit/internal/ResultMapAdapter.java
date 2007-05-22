package org.cs3.pl.profit.internal;

import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import fit.Binding;
import fit.Fixture;
import fit.TypeAdapter;

public class ResultMapAdapter extends TypeAdapter implements ResultMapProvider {

	private static class ResultAdapter extends ResultMapAdapter {

		public static boolean canHandle(String name) {
			return "result".equals(name);
		}

		public ResultAdapter(Fixture fixture) {
			init(fixture, Map.class);
		}

		@Override
		public Object get() {
			return result();
		}

		@Override
		public Object parse(String text) throws Exception {
			try {
				StringTokenizer tokenizer = new StringTokenizer(text.substring(
						1, text.length() - 1), ",");
				Map<String, String> map = new HashMap<String, String>();
				while (tokenizer.hasMoreTokens()) {
					String next = tokenizer.nextToken().trim();
					int pos = next.indexOf("=");
					String key = next.substring(0, pos);
					String value = next.substring(pos + 1);
					map.put(key, value);
				}
				return map;
			} catch (Exception e) {
				throw new RuntimeException("Could not parse map. \n"
						+ "Reason: " + e.getMessage());
			}
		}

	}

	private static class SuccessAdapter extends ResultMapAdapter {

		private static final TypeAdapter BOOLEAN_PARSER //
		= TypeAdapter.adapterFor(Boolean.class);

		public static boolean canHandle(String name) {
			return "success".equals(name);
		}

		public SuccessAdapter(Fixture fixture) {
			init(fixture, Boolean.class);
		}

		@Override
		public Object get() {
			return (result() != null);
		}

		@Override
		public Object parse(String s) throws Exception {
			return BOOLEAN_PARSER.parse(s);
		}

	}

	private static class VariableAdapter extends ResultMapAdapter {

		public static boolean canHandle(String name) {
			return Character.isUpperCase(name.charAt(0));
		}

		private String variable;

		public VariableAdapter(Fixture fixture, String name) {
			init(fixture, String.class);
			this.variable = name;
		}

		@Override
		public Object get() {
			return result().get(variable);
		}

	}

	public static boolean canHandle(String columnHead) {
		String name = extractName(columnHead);
		return (name != null) && (ResultAdapter.canHandle(name) || // 
				SuccessAdapter.canHandle(name) || // 
				VariableAdapter.canHandle(name));
	}

	public static Binding createResultMapBinding(Fixture fixture,
			String columnHead, ResultMapProvider provider) {
		ResultMapAdapter adapter = on(fixture, columnHead);
		adapter.resultMapProvider = (provider != null) ? provider : adapter;
		Binding binding = new Binding.QueryBinding();
		binding.adapter = adapter;
		return binding;
	}

	private static String extractName(String columnHead) {
		if (columnHead.endsWith("?")) {
			return columnHead.substring(0, columnHead.length() - 1);
		} else if (columnHead.endsWith("()")) {
			return columnHead.substring(0, columnHead.length() - 2);
		} else
			return null;
	}

	public static ResultMapAdapter on(Fixture fixture, String columnHead) {
		String name = extractName(columnHead);
		if (SuccessAdapter.canHandle(name))
			return new SuccessAdapter(fixture);
		else if (ResultAdapter.canHandle(name))
			return new ResultAdapter(fixture);
		else
			return new VariableAdapter(fixture, name);
	}

	ResultMapProvider resultMapProvider;

	public Map result() {
		return resultMapProvider.getResultMap();
	}

	public Map getResultMap() {
		return (Map) target;
	}

}
