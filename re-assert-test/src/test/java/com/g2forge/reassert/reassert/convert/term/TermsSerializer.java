package com.g2forge.reassert.reassert.convert.term;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

public class TermsSerializer extends StdSerializer<ITerms<? extends ITerm>> {
	private static final long serialVersionUID = 3930570817711728930L;

	protected TermsSerializer() {
		super(ITerms.class, true);
	}

	@Override
	public void serialize(ITerms<? extends ITerm> value, JsonGenerator generator, SerializerProvider provider) throws IOException {
		generator.getCodec().writeValue(generator, toStored(value));
	}

	protected <T extends ITerm> List<StoredTerm> toStored(ITerms<T> value) {
		return value.getTerms(true).stream().map(term -> new StoredTerm(term, value.getRelation(term))).collect(Collectors.toList());
	}
}