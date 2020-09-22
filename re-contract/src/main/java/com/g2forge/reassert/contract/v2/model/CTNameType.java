package com.g2forge.reassert.contract.v2.model;

import com.g2forge.reassert.core.model.contract.terms.ITerm;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class CTNameType implements ICTName {
	protected final ITerm term;

	@Getter(lazy = true)
	@EqualsAndHashCode.Exclude
	@ToString.Exclude
	private final ContractType contractType = ContractType.valueOf(getTerm());
}
