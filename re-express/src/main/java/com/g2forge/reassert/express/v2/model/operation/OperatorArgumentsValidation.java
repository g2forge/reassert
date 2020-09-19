package com.g2forge.reassert.express.v2.model.operation;

import com.g2forge.alexandria.java.validate.IValidation;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class OperatorArgumentsValidation implements IValidation {
	protected final boolean valid;
}
