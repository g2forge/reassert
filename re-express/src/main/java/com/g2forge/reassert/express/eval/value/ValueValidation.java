package com.g2forge.reassert.express.eval.value;

import com.g2forge.alexandria.java.validate.IValidation;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ValueValidation<Value> implements IValidation {
	protected final Value value;

	protected final boolean valid;
}
