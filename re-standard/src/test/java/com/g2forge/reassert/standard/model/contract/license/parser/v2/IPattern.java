package com.g2forge.reassert.standard.model.contract.license.parser.v2;

import com.g2forge.alexandria.java.fluent.optional.IOptional;

public interface IPattern<Result> {
	public IOptional<Result> match(String string);
}
