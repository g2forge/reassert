package com.g2forge.reassert.standard.model.contract.license.parser.v2;

import com.g2forge.alexandria.java.function.builder.IBuilder;

public interface ICharacterClassBuilder<Result> extends IBuilder<Result> {
	public ICharacterClassBuilder<Result> character(char character);

	public ICharacterClassBuilder<Result> range(char start, char end);
}
