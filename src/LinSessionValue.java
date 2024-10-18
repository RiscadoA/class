public class LinSessionValue extends SessionField {
    LinSession lin;

    public LinSessionValue(String id)
    {
	lin = new LinSession(id);
    }

    LinSession getLin() {
	return lin;
    }
    

}
