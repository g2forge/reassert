package com.g2forge.reassert.core.api.parser;

@FunctionalInterface
public interface IParser<T> {
	public T parse(String text);
}
