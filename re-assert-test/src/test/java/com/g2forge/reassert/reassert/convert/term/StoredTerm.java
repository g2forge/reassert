package com.g2forge.reassert.reassert.convert.term;

import com.g2forge.reassert.core.model.contract.ITerm;
import com.g2forge.reassert.core.model.contract.TermRelation;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class StoredTerm {
	protected final ITerm term;

	protected final TermRelation relation;
}