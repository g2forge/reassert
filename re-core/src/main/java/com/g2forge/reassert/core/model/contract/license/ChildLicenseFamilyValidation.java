package com.g2forge.reassert.core.model.contract.license;

import com.g2forge.alexandria.java.validate.IValidation;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ChildLicenseFamilyValidation implements IValidation {
	protected final boolean valid;
}
