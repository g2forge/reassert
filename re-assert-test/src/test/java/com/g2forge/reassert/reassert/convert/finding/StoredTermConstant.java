package com.g2forge.reassert.reassert.convert.finding;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.g2forge.reassert.core.model.contract.ITerm;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class StoredTermConstant {
	@JsonProperty("@c")
	protected final String type;
	
	protected final ITerm term;

	protected final String contract;
}
